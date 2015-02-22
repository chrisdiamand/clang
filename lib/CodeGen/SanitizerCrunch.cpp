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
                             CheckFunction *CheckFunPtr)
{
  clang::LangOptions langOpts;
  clang::PrintingPolicy printPol(langOpts);
  CheckFunction CheckFun = CT_Unknown;
  std::string Ret = "__UNKNOWN_TYPE__";

  // Based on TypePrinting::print()
  if (Ty->isBuiltinType()) {
    auto BTy = clang::cast<clang::BuiltinType>(Ty);
    if (BTy->isVoidType()) {
      CheckFun = CT_PointerOfDegree;
    } else {
      CheckFun = CT_IsA;
    }
    Ret = BTy->getName(printPol).str();

  } else if (Ty->isRecordType()) {
    auto RTy = Ty->getAsStructureType();
    Ret = RTy->getDecl()->getName().str();
    CheckFun = CT_Named;

  } else if (Ty->isPointerType()) {
    clang::QualType PtrTy = Ty->getPointeeType();
    Ret = "__PTR_" + parseType(PtrTy, &CheckFun);

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
    CheckFun = CT_FunctionRefining;

  } else {
    std::cerr << "Unknown type class: ";
    Ty->dump();
  }

  if (CheckFunPtr != nullptr) {
    *CheckFunPtr = CheckFun;
  }

  return Ret;
}

// GetUniqtype - return the correct uniqtype variable for a given type to
// check.
llvm::Value *Check::getUniqtypeVariable(clang::QualType &QTy) {
  std::string UniqtypeName = "__uniqtype__" + CrunchTypeName;

  llvm::Module &TheModule = CGF.CGM.getModule();
  llvm::Type *utTy = llvm::Type::getInt8PtrTy(VMContext);
  llvm::Constant *ret = TheModule.getOrInsertGlobal(UniqtypeName, utTy);
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

  CrunchTypeName = parseType(PointeeTy, &CheckFun);
}

void Check::Emit() {
  if (!CGF.SanOpts.has(SanitizerKind::Crunch))
    return;

  DstTy = CGF.ConvertType(DestClangTy);

  llvm::Module &TheModule = CGF.CGM.getModule();
  llvm::Type *resTy = llvm::Type::getInt32Ty(VMContext);

  llvm::Type *argTy[2];
  argTy[0] = llvm::Type::getInt8PtrTy(VMContext);
  argTy[1] = llvm::Type::getInt8PtrTy(VMContext);
  llvm::ArrayRef<llvm::Type *> ArgTy_ar(const_cast<llvm::Type **>(argTy), 2);
  llvm::FunctionType *CheckT = llvm::FunctionType::get(resTy, ArgTy_ar, false);
  llvm::Constant *CheckF = TheModule.getOrInsertFunction("__is_a_internal",
                                                         CheckT);

  assert(CheckF != NULL && "__is_aU not declared!");

  // Cast the pointer to int8_t * to match __is_aU().
  Src = Builder.CreateBitCast(Src, argTy[0]);

  std::vector<llvm::Value *> ArgsV;
  ArgsV.push_back(Src);

  ArgsV.push_back(getUniqtypeVariable(DestClangTy));

  Builder.CreateCall(CheckF, ArgsV, "crunchcheck");
}

void EmitCastCheck(CodeGenFunction &CGF, CGBuilderTy &Builder,
                   llvm::LLVMContext &VMContext,
                   llvm::Value *Src, clang::QualType &DestClangTy)
{
  Check c(CGF, Builder, VMContext, Src, DestClangTy);
  c.Emit();
}

} // namespace Crunch
