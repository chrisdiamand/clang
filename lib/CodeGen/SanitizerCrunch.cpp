#include <assert.h>

#include "CodeGenFunction.h"
#include "CGCXXABI.h"
#include "CGDebugInfo.h"
#include "CGObjCRuntime.h"
#include "CodeGenModule.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/RecordLayout.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/CodeGenOptions.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include <cstring>
#include <iostream>

#include "SanitizerCrunch.h"

using namespace clang;
using namespace CodeGen;

namespace Crunch {

/* Recurse down the type structure, returning the string used by libcrunch to
 * represent that type, and finding out which libcrunch function needs to be
 * called to check it. */
static std::string parseType(const clang::QualType &Ty,
                             CheckFunctionKind *CheckFunResult,
                             int *PointerDegree)
{
  clang::LangOptions langOpts;
  clang::PrintingPolicy printPol(langOpts);
  CheckFunctionKind CheckFunKind = CT_IsA;
  std::string Ret = "__UNKNOWN_TYPE__";

  // Based on TypePrinting::print()
  if (Ty->isBuiltinType()) {
    auto BTy = clang::cast<clang::BuiltinType>(Ty);
    if (BTy->isVoidType()) {
      CheckFunKind = CT_PointerOfDegree;
    } else {
      CheckFunKind = CT_IsA;
    }
    Ret = BTy->getName(printPol).str();

  } else if (Ty->isRecordType()) {
    auto RTy = Ty->getAsStructureType();
    Ret = RTy->getDecl()->getName().str();
    CheckFunKind = CT_Named;

  } else if (Ty->isPointerType()) {
    clang::QualType PtrTy = Ty->getPointeeType();
    Ret = "__PTR_" + parseType(PtrTy, &CheckFunKind, nullptr);

    if (PointerDegree != nullptr) {
      *PointerDegree += 1;
    }

  } else if (Ty->isFunctionProtoType()) {
    auto FTy = clang::cast<const clang::FunctionProtoType>(Ty);
    Ret = "__FUN_FROM_";

    int NumParams = FTy->getNumParams();
    for (int i = 0; i < NumParams; ++i) {
      Ret += "__ARG" + std::to_string(i) + "_";
      Ret += parseType(FTy->getParamType(i), nullptr, nullptr);
    }

    auto ReturnType = FTy->getReturnType();
    Ret += "__FUN_TO_" + parseType(ReturnType, nullptr, nullptr);
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

// GetUniqtype - return the correct uniqtype variable for a given type to
// check.
llvm::Value *Check::getUniqtypeVariable() {
  switch (CheckFunKind) {
    case CT_IsA:
    case CT_FunctionRefining: {
      std::string UniqtypeName = "__uniqtype__" + CrunchTypeName;

      llvm::Type *utTy = llvm::Type::getInt8PtrTy(VMContext);
      llvm::Constant *ret = getModule().getOrInsertGlobal(UniqtypeName, utTy);
      return Builder.CreateBitCast(ret, utTy);
    }

    case CT_Named:
      return llvm::ConstantDataArray::getString(VMContext, CrunchTypeName);

    case CT_PointerOfDegree:
      return llvm::ConstantInt::get(llvm::Type::getInt32Ty(VMContext),
                                    PointerDegree);
  }

  return nullptr;
}

Check::Check(clang::CodeGen::CodeGenFunction &_CGF,
             clang::CodeGen::CGBuilderTy &_Builder,
             llvm::LLVMContext &_VMContext,
             llvm::Value *_Src, clang::QualType &_DestClangTy) :
  CGF(_CGF), Builder(_Builder), VMContext(_VMContext),
  Src(_Src), DestClangTy(_DestClangTy)
{
  assert(DestClangTy->isPointerType() && "Can't check non-pointer destination types");
  PointeeTy = DestClangTy->getPointeeType();
  /* Need to strip parentheses; these occur around function prototypes and mean
   * that the QualType can't be casted directly to a FunctionProtoType. */
  PointeeTy = PointeeTy.IgnoreParens();

  PointerDegree = 0;
  CrunchTypeName = parseType(PointeeTy, &CheckFunKind, &PointerDegree);
}

static std::string getCheckFunctionName(CheckFunctionKind Kind) {
  switch (Kind) {
    case CT_IsA:                return "__is_a_internal";
    case CT_Named:              return "__named_a_internal";
    case CT_PointerOfDegree:    return "__is_a_pointer_of_degree_internal";
    case CT_FunctionRefining:   return "__is_a_function_refining_internal";
  }
  assert(false && "Invalid CheckFunctionKind");
  return "ERROR";
}

llvm::Constant *Check::getCheckFunction(llvm::Type *SecondArg) {
  llvm::Constant *Ret = nullptr;
  llvm::Type *ArgTy[2];
  llvm::Module &TheModule = getModule();
  llvm::Type *ResTy = llvm::Type::getInt32Ty(VMContext);
  std::string FunName = getCheckFunctionName(CheckFunKind);

  // The first argument is always the pointer to be checked.
  ArgTy[0] = llvm::Type::getInt8PtrTy(VMContext);
  ArgTy[1] = SecondArg;

  auto ArgTyPtr = const_cast<llvm::Type **>(ArgTy);
  llvm::ArrayRef<llvm::Type *> ArgTy_ar(ArgTyPtr, 2);
  llvm::FunctionType *FunTy = llvm::FunctionType::get(ResTy, ArgTy_ar, false);
  Ret = TheModule.getOrInsertFunction(FunName, FunTy);

  return Ret;
}

void Check::Emit() {
  if (!CGF.SanOpts.has(SanitizerKind::Crunch))
    return;

  // Cast the pointer to int8_t * to match __is_aU().
  Src = Builder.CreateBitCast(Src, llvm::Type::getInt8PtrTy(VMContext));

  std::vector<llvm::Value *> ArgsV;
  ArgsV.push_back(Src);

  llvm::Value *Uniqtype = getUniqtypeVariable();
  llvm::Constant *CheckFun = getCheckFunction(Uniqtype->getType());

  ArgsV.push_back(Uniqtype);

  Builder.CreateCall(CheckFun, ArgsV, "crunchcheck");
}

void EmitCastCheck(CodeGenFunction &CGF, CGBuilderTy &Builder,
                   llvm::LLVMContext &VMContext,
                   llvm::Value *Src, clang::QualType &DestClangTy)
{
  Check c(CGF, Builder, VMContext, Src, DestClangTy);
  c.Emit();
}

} // namespace Crunch
