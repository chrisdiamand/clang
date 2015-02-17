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

static std::string getUniqtypeName(const clang::QualType &Ty) {
  clang::LangOptions langOpts;
  clang::PrintingPolicy printPol(langOpts);

  // Based on TypePrinting::print()
  if (Ty->isBuiltinType()) {
    auto BTy = clang::cast<clang::BuiltinType>(Ty);
    return BTy->getName(printPol).str();

  } else if (Ty->isRecordType()) {
    auto RTy = Ty->getAsStructureType();
    return RTy->getDecl()->getName().str();

  } else if (Ty->isPointerType()) {
    clang::QualType PtrTy = Ty->getPointeeType();
    return "__PTR_" + getUniqtypeName(PtrTy);

  } else if (Ty->isFunctionProtoType()) {
    auto FTy = clang::cast<const clang::FunctionProtoType>(Ty);
    std::string Ret = "__FUN_FROM_";

    int NumParams = FTy->getNumParams();
    for (int i = 0; i < NumParams; ++i) {
      Ret += "__ARG" + std::to_string(i) + "_";
      Ret += getUniqtypeName(FTy->getParamType(i));
    }

    auto ReturnType = FTy->getReturnType();
    Ret += "__FUN_TO_" + getUniqtypeName(ReturnType);
    return Ret;
  }

  std::cerr << "Unknown type class: ";
  Ty->dump();
  return "__UNKNOWN_TYPE__";
}

// GetUniqtype - return the correct uniqtype variable for a given type to
// check.
llvm::Value *Check::GetUniqtype(clang::QualType &QTy) {
  assert(QTy->isPointerType() && "Can't check non-pointer destination types");

  clang::QualType ptrTy = QTy->getPointeeType();
  /* Need to strip parentheses; these occur around function prototypes and mean
   * that the QualType can't be casted directly to a FunctionProtoType. */
  ptrTy = ptrTy.IgnoreParens();

  std::string UniqtypeName = "__uniqtype__" + getUniqtypeName(ptrTy);

  llvm::Module &TheModule = CGF.CGM.getModule();
  llvm::Type *utTy = llvm::Type::getInt8PtrTy(VMContext);
  llvm::Constant *ret = TheModule.getOrInsertGlobal(UniqtypeName, utTy);
  return Builder.CreateBitCast(ret, utTy);
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

  ArgsV.push_back(GetUniqtype(DestClangTy));

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
