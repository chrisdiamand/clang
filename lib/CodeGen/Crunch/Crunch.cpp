#include <assert.h>

#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Module.h"

#include <cstring>
#include <iostream>
#include <unordered_map>

#include "Crunch/Check.h"
#include "Crunch/Crunch.h"
#include "Crunch/Emit.h"

using namespace clang;
using namespace CodeGen;

namespace Crunch {

using clang::BuiltinType;

static std::string getBuiltinTypeName(const BuiltinType *Ty) {
  switch (Ty->getKind()) {
    case BuiltinType::Void:       return "void";
    case BuiltinType::Bool:       return "bool";
    case BuiltinType::Char_S:     return "signed_char";
    case BuiltinType::Char_U:     return "unsigned_char";
    case BuiltinType::SChar:      return "signed_char";
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

struct TypeParse {
  std::string         UniqtypeName;
  CheckFunctionKind   CheckFunKind;
  int                 PointerDegree;
};

/* Recurse down the type structure, returning the string used by libcrunch to
 * represent that type, and finding out which libcrunch function needs to be
 * called to check it. */
static TypeParse parseType_actual(const clang::QualType &NonCanonicalTy) {
  // Remove typedefs.
  auto Ty = NonCanonicalTy.getCanonicalType();

  static std::unordered_map<const clang::Type *, TypeParse> History;
  const clang::Type *TypePtr = Ty.getTypePtr();
  if (History.find(TypePtr) != History.end()) {
    return History[TypePtr];
  }

  clang::LangOptions langOpts;
  clang::PrintingPolicy printPol(langOpts);
  TypeParse Ret;
  Ret.UniqtypeName = "__UNKNOWN_TYPE__";
  Ret.CheckFunKind = CT_IsA;
  Ret.PointerDegree = 0;

  if (auto ArrayTy = dyn_cast<clang::ArrayType>(Ty)) {
    Ty = ArrayTy->getElementType();
  }

  if (Ty->isBuiltinType()) {
    auto BTy = clang::cast<clang::BuiltinType>(Ty);
    if (BTy->isVoidType()) {
      Ret.CheckFunKind = CT_PointerOfDegree;
    } else {
      Ret.CheckFunKind = CT_IsA;
    }
    Ret.UniqtypeName = getBuiltinTypeName(BTy);

  } else if (Ty->isRecordType()) {
    auto RTy = Ty->getAsStructureType();
    auto Decl = RTy->getDecl();
    Ret.UniqtypeName = Decl->getName().str();
    /* Crunchcc generates a __named_a check when there is no definition, or a
     * __is_a check otherwise. */
    if (Decl->getDefinition() == nullptr) {
      Ret.CheckFunKind = CT_Named;
    } else {
      Ret.CheckFunKind = CT_IsA;
    }
    Ret.PointerDegree = 0;

  } else if (Ty->isPointerType()) {
    clang::QualType PtrTy = Ty->getPointeeType();
    TypeParse Pointee = parseType_actual(PtrTy);
    Ret.UniqtypeName = "__PTR_" + Pointee.UniqtypeName;
    Ret.CheckFunKind = Pointee.CheckFunKind;
    Ret.PointerDegree = Pointee.PointerDegree + 1;

  } else if (Ty->isFunctionProtoType()) {
    auto FTy = clang::cast<const clang::FunctionProtoType>(Ty);
    Ret.UniqtypeName = "__FUN_FROM_";

    int NumParams = FTy->getNumParams();
    for (int i = 0; i < NumParams; ++i) {
      Ret.UniqtypeName += "__ARG" + std::to_string(i) + "_";
      Ret.UniqtypeName += parseType_actual(FTy->getParamType(i)).UniqtypeName;
    }

    auto ReturnType = FTy->getReturnType();
    Ret.UniqtypeName += "__FUN_TO_"
                     + parseType_actual(ReturnType).UniqtypeName;
    Ret.CheckFunKind = CT_FunctionRefining;
    Ret.PointerDegree = 0;

  } else {
    std::cerr << "Unknown type class: ";
    Ty->dump();
  }

  assert(Ret.CheckFunKind == CT_IsA ||
         Ret.CheckFunKind == CT_Named ||
         Ret.CheckFunKind == CT_FunctionRefining ||
         Ret.CheckFunKind == CT_PointerOfDegree);

  History[TypePtr] = Ret;
  return Ret;
}

// Wrapper to skip checks to 'void *' and 'char *'.
std::string parseType(const clang::QualType &Ty,
                      CheckFunctionKind *CheckFunResult,
                      int *PointerDegree)
{
  if (Ty->isVoidType() || Ty->isCharType()) {
    if (CheckFunResult) {
      *CheckFunResult = CT_NoCheck;
    }
    return "ERROR";
  }

  TypeParse Parse = parseType_actual(Ty);

  if (CheckFunResult) {
    *CheckFunResult = Parse.CheckFunKind;
  }

  if (PointerDegree) {
    *PointerDegree = Parse.PointerDegree;
  }

  return Parse.UniqtypeName;
}

std::string getUniqtypeName(const clang::QualType &Ty) {
  return "__uniqtype__" + parseType_actual(Ty).UniqtypeName;
}

void emitCastCheck(CodeGenFunction &CGF, const clang::Expr *ClangSrc,
                   llvm::Value *Src, clang::QualType &DestClangTy)
{
  Check c(CGF, ClangSrc, Src, DestClangTy);
  c.emit();
}

llvm::Constant *getSizeofFunction(CodeGen::CodeGenFunction &CGF,
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
llvm::Value *markSizeofExpr(CodeGen::CodeGenFunction &CGF,
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
    ArgType = SizeofExpr->getTypeOfArgument();

  } else if (auto OffsetOfExpr = dyn_cast<clang::OffsetOfExpr>(E)) {
    ArgType = OffsetOfExpr->getTypeSourceInfo()->getType();
  } else {
    return ActualValue;
  }

  std::string TypeDesc = parseType_actual(ArgType).UniqtypeName;

  if (TypeDesc.size() == 0) { // Anonymous struct, for example.
    return ActualValue;
  }

  llvm::Value *Args[2];
  Args[0] = llvm::ConstantDataArray::getString(CGF.getLLVMContext(), TypeDesc);
  Args[1] = ActualValue;
  llvm::Constant *Fun = getSizeofFunction(CGF, Args);

  return CGF.Builder.CreateCall(Fun, Args);
}

static void getArgValues(const CodeGen::CallArgList &ArgList,
                         std::vector<llvm::Value *> *Ret)
{
  for (const CodeGen::CallArg &Arg : ArgList) {
    if (Arg.RV.isScalar()) {
      Ret->push_back(Arg.RV.getScalarVal());
    } else if (Arg.RV.isAggregate()) {
      Ret->push_back(Arg.RV.getAggregatePointer());
    } else { // if (RV.isComplex())
      assert(false && "RValue not scalar or aggregate?");
    }
  }
}

static llvm::Constant *getCheckArgsFun(CodeGen::CodeGenFunction &CGF,
                                       const std::vector<llvm::Value *> &Args)
{
  llvm::Module &TheModule = CGF.CGM.getModule();

  std::vector<llvm::Type *> ArgTy;
  for (auto it = Args.begin(); it != Args.end(); ++it) {
    ArgTy.push_back((*it)->getType());
  }
  llvm::Type *RetTy = llvm::Type::getInt32Ty(CGF.getLLVMContext());

  llvm::ArrayRef<llvm::Type *> ArgTy_ar(ArgTy);
  llvm::FunctionType *FunTy = llvm::FunctionType::get(RetTy, ArgTy_ar, false);

  return TheModule.getOrInsertFunction("__check_args_internal", FunTy);
}

void checkCallArgs(CodeGen::CodeGenFunction &CGF, const clang::CallExpr *E,
                   llvm::Value *Callee, CodeGen::CallArgList &ArgList)
{
  if (!isEnabled(CGF) || !sloppyFunctionPointers() ||
      E->getDirectCallee() != nullptr) // Ignore direct calls.
    return;

  std::vector<llvm::Value *> Args;
  Args.push_back(Callee);

  auto IntTy = llvm::Type::getInt32Ty(CGF.getLLVMContext());
  Args.push_back(llvm::ConstantInt::get(IntTy, ArgList.size()));

  getArgValues(ArgList, &Args);

  llvm::Constant *Fun = getCheckArgsFun(CGF, Args);
  llvm::Value *CheckRet = CGF.Builder.CreateCall(Fun, Args, "args_check");

  emitAssert(CGF, CheckRet, "__check_args_internal()", E->getExprLoc());
}

// Emit an __is_a check on the return value.
void checkCallRet(CodeGen::CodeGenFunction &CGF,
                  const clang::CallExpr *E, llvm::Value *Ret)
{
  if (!isEnabled(CGF) || !sloppyFunctionPointers() ||
      E->getDirectCallee() != nullptr) // Ignore direct calls.
    return;

  clang::QualType RetType = E->getCallReturnType(CGF.getContext());
  if (!RetType->isPointerType()) { // We can't check non-pointer return types.
    return;
  }

  emitCastCheck(CGF, E, Ret, RetType);
}

} // namespace Crunch
