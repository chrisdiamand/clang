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
                             CheckFunctionKind *CheckFunResult)
{
  clang::LangOptions langOpts;
  clang::PrintingPolicy printPol(langOpts);
  CheckFunctionKind CheckFunKind = CT_Unknown;
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
    Ret = "__PTR_" + parseType(PtrTy, &CheckFunKind);

  } else if (Ty->isFunctionProtoType()) {
    auto FTy = clang::cast<const clang::FunctionProtoType>(Ty);
    Ret = "__FUN_FROM_";

    int NumParams = FTy->getNumParams();
    for (int i = 0; i < NumParams; ++i) {
      Ret += "__ARG" + std::to_string(i) + "_";
      Ret += parseType(FTy->getParamType(i), nullptr);
    }

    auto ReturnType = FTy->getReturnType();
    Ret += "__FUN_TO_" + parseType(ReturnType, nullptr);
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
  std::string UniqtypeName = "__uniqtype__" + CrunchTypeName;

  llvm::Type *utTy = llvm::Type::getInt8PtrTy(VMContext);
  llvm::Constant *ret = getModule().getOrInsertGlobal(UniqtypeName, utTy);
  return Builder.CreateBitCast(ret, utTy);
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

  CrunchTypeName = parseType(PointeeTy, &CheckFunKind);
}

llvm::Constant *Check::getCheckFunction() {
  llvm::Constant *Ret = nullptr;
  llvm::Type *ArgTy[2], *ResTy;
  auto ArgTyPtr = const_cast<llvm::Type **>(ArgTy);
  llvm::FunctionType *FunTy;
  llvm::Module &TheModule = getModule();

  switch (CheckFunKind) {
  case CT_Unknown:
    return nullptr;

  default:
  case CT_IsA: {
      ResTy = llvm::Type::getInt32Ty(VMContext);

      ArgTy[0] = llvm::Type::getInt8PtrTy(VMContext);
      ArgTy[1] = llvm::Type::getInt8PtrTy(VMContext);
      llvm::ArrayRef<llvm::Type *> ArgTy_ar(ArgTyPtr, 2);
      FunTy = llvm::FunctionType::get(ResTy, ArgTy_ar, false);
      Ret = TheModule.getOrInsertFunction("__is_a_internal", FunTy);

      break;
    }
  }

  return Ret;
}

void Check::Emit() {
  if (!CGF.SanOpts.has(SanitizerKind::Crunch))
    return;

  llvm::Constant *CheckFun = getCheckFunction();

  // Cast the pointer to int8_t * to match __is_aU().
  Src = Builder.CreateBitCast(Src, llvm::Type::getInt8PtrTy(VMContext));

  std::vector<llvm::Value *> ArgsV;
  ArgsV.push_back(Src);

  ArgsV.push_back(getUniqtypeVariable());

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
