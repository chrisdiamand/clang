#include <assert.h>

#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Module.h"

#include <cstring>
#include <iostream>

#include "Crunch/Check.h"
#include "Crunch/Crunch.h"

using namespace clang;
using namespace CodeGen;

namespace Crunch {

using clang::BuiltinType;

static std::string getBuiltinTypeName(const BuiltinType *Ty) {
  switch (Ty->getKind()) {
    case BuiltinType::Void:       return "void";
    case BuiltinType::Bool:       return "bool";
    case BuiltinType::Char_S:     return "char";
    case BuiltinType::Char_U:     return "unsigned_char";
    case BuiltinType::SChar:      return "char";
    case BuiltinType::UChar:      return "unsigned_char";
    case BuiltinType::WChar_S:    return "wchar_t";
    case BuiltinType::WChar_U:    return "unsigned_wchar_t";
    case BuiltinType::Char16:     return "char16_t";
    case BuiltinType::Char32:     return "char32_t";
    case BuiltinType::UShort:     return "unsigned_short_int";
    case BuiltinType::Short:      return "short_int";
    case BuiltinType::UInt:       return "unsigned_int";
    case BuiltinType::Int:        return "int";
    case BuiltinType::ULong:      return "unsigned_long_int";
    case BuiltinType::Long:       return "long_int";
    case BuiltinType::ULongLong:  return "unsigned_long_long_int";
    case BuiltinType::LongLong:   return "long_long_int";
    case BuiltinType::Int128:     return "int128_t";
    case BuiltinType::UInt128:    return "uint128_t";
    case BuiltinType::Half:       return "__fp16";
    case BuiltinType::Float:      return "float";
    case BuiltinType::Double:     return "double";
    case BuiltinType::LongDouble: return "long_double";
    case BuiltinType::NullPtr:    return "__PTR_void";
    default:                      break;
  }

  return "UNKNOWN";
}

/* Recurse down the type structure, returning the string used by libcrunch to
 * represent that type, and finding out which libcrunch function needs to be
 * called to check it. */
static std::string parseType_actual(const clang::QualType &NonCanonicalTy,
                                    CheckFunctionKind *CheckFunResult,
                                    int *PointerDegree)
{
  clang::LangOptions langOpts;
  clang::PrintingPolicy printPol(langOpts);
  CheckFunctionKind CheckFunKind = CT_IsA;
  std::string Ret = "__UNKNOWN_TYPE__";

  // Remove typedefs.
  auto Ty = NonCanonicalTy.getCanonicalType();

  if (Ty->isBuiltinType()) {
    auto BTy = clang::cast<clang::BuiltinType>(Ty);
    if (BTy->isVoidType()) {
      CheckFunKind = CT_PointerOfDegree;
    } else {
      CheckFunKind = CT_IsA;
    }
    Ret = getBuiltinTypeName(BTy);

  } else if (Ty->isRecordType()) {
    auto RTy = Ty->getAsStructureType();
    auto Decl = RTy->getDecl();
    Ret = Decl->getName().str();
    /* Crunchcc generates a __named_a check when there is no definition, or a
     * __is_a check otherwise. */
    if (Decl->getDefinition() == nullptr) {
      CheckFunKind = CT_Named;
    } else {
      CheckFunKind = CT_IsA;
    }

  } else if (Ty->isPointerType()) {
    clang::QualType PtrTy = Ty->getPointeeType();
    Ret = "__PTR_" + parseType_actual(PtrTy, &CheckFunKind, nullptr);

    if (PointerDegree != nullptr) {
      *PointerDegree += 1;
    }

  } else if (Ty->isFunctionProtoType()) {
    auto FTy = clang::cast<const clang::FunctionProtoType>(Ty);
    Ret = "__FUN_FROM_";

    int NumParams = FTy->getNumParams();
    for (int i = 0; i < NumParams; ++i) {
      Ret += "__ARG" + std::to_string(i) + "_";
      Ret += parseType_actual(FTy->getParamType(i), nullptr, nullptr);
    }

    auto ReturnType = FTy->getReturnType();
    Ret += "__FUN_TO_" + parseType_actual(ReturnType, nullptr, nullptr);
    CheckFunKind = CT_FunctionRefining;

  } else {
    std::cerr << "Unknown type class: ";
    Ty->dump();
  }

  if (CheckFunResult != nullptr) {
    *CheckFunResult = CheckFunKind;
  }

  return Ret;
}

// Wrapper to skip checks to 'void *' and 'char *'.
std::string parseType(const clang::QualType &Ty,
                             CheckFunctionKind *CheckFunResult,
                             int *PointerDegree)
{
  if (PointerDegree) {
    *PointerDegree = 0;
  }

  if (CheckFunResult) {
    *CheckFunResult = CT_NoCheck;
  }

  if (!Ty->isVoidType() && !Ty->isCharType()) {
    return parseType_actual(Ty, CheckFunResult, PointerDegree);
  }

  return "ERROR";
}

std::string getUniqtypeName(const clang::QualType &Ty) {
  return "__uniqtype__" + parseType_actual(Ty, nullptr, nullptr);
}

void emitCastCheck(CodeGenFunction &CGF, clang::Expr *ClangSrc,
                   llvm::Value *Src, clang::QualType &DestClangTy)
{
  Check c(CGF, ClangSrc, Src, DestClangTy);
  c.emit();
}

llvm::Constant *getSizeofFunction(clang::CodeGen::CodeGenFunction &CGF,
                                  llvm::Value **Args)
{
  llvm::Type *ArgTy[2];
  llvm::Module &TheModule = CGF.CGM.getModule();
  const std::string FunName = "__crunch_sizeof__";
  ArgTy[0] = Args[0]->getType();
  // This function will return its second argument, so the types need to match.
  ArgTy[1] = Args[1]->getType();
  llvm::Type *RetTy = ArgTy[1];

  llvm::ArrayRef<llvm::Type *> ArgTy_ar(const_cast<llvm::Type **>(ArgTy), 2);
  llvm::FunctionType *FunTy = llvm::FunctionType::get(RetTy, ArgTy_ar, false);

  return TheModule.getOrInsertFunction(FunName, FunTy);
}

/* We need to preserve sizeof expressions (instead of just returning a number)
 * so that their type ends up in the LLVM IR. */
llvm::Value *markSizeofExpr(clang::CodeGen::CodeGenFunction &CGF,
                            const clang::Expr *E, llvm::Value *ActualValue)
{
  if (!isEnabled(CGF)) {
    return ActualValue;
  }

  clang::QualType ArgType;

  if (auto SizeofExpr = dyn_cast<clang::UnaryExprOrTypeTraitExpr>(E)) {
    auto Kind = SizeofExpr->getKind();
    if (Kind != clang::UETT_SizeOf) {
      return ActualValue;
    }
    ArgType = SizeofExpr->getArgumentType();

  } else if (auto OffsetOfExpr = dyn_cast<clang::OffsetOfExpr>(E)) {
    ArgType = OffsetOfExpr->getTypeSourceInfo()->getType();
  }

  std::string TypeDesc = parseType_actual(ArgType, nullptr, nullptr);

  llvm::Value *Args[2];
  Args[0] = llvm::ConstantDataArray::getString(CGF.getLLVMContext(), TypeDesc);
  Args[1] = ActualValue;
  llvm::Constant *Fun = getSizeofFunction(CGF, Args);

  return CGF.Builder.CreateCall(Fun, Args);
}

} // namespace Crunch
