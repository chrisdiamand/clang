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

static std::string getUniqtypeName(const clang::Type *Ty) {
  const clang::LangOptions langOpts;
  const clang::PrintingPolicy printPol(langOpts);

  // Based on TypePrinting::print()
  if (Ty->isBuiltinType()) {
    auto *BTy = static_cast<const clang::BuiltinType *>(Ty);
    return BTy->getName(printPol).str();

  } else if (Ty->isRecordType()) {
    auto RTy = Ty->getAsStructureType();
    return RTy->getDecl()->getName().str();

  } else if (Ty->isPointerType()) {
    const clang::Type *PtrTy = Ty->getPointeeType().getTypePtr();
    return "__PTR_" + getUniqtypeName(PtrTy);

  } else if (Ty->isFunctionProtoType()) {
    auto FTy = static_cast<const clang::FunctionProtoType *>(Ty);
    assert(FTy != NULL);
    std::string Ret = "__FUN_FROM_";
    int NumParams = FTy->getNumParams();
    printf("numparams = %d\n", NumParams);
    for (int i = 0; i < NumParams; ++i) {
      printf("ARGUMENT %d\n", i);
      //QualType ArgType = *it;
      Ret += "_ARG" + std::to_string(i) + "_";
      //Ret += getUniqtypeName(ArgType.getTypePtrOrNull());
    }

    fprintf(stderr, "Getting return type: ");
    auto ReturnType = FTy->getReturnType().getTypePtrOrNull();
    fprintf(stderr, "ptr = %p\n", ReturnType);
    //ReturnType->dump();
    fprintf(stderr, "Done, getting ptr:\n");
    //std::cerr << getUniqtypeName(ReturnType);
    //Ret += "__FUN_TO_" + getUniqtypeName(ReturnType);
    return Ret;
  }

  std::cerr << "Unknown type class: ";
  Ty->dump();
  return "__UNKNOWN_TYPE__";
}

// GetUniqtype - return the correct uniqtype variable for a given type to
// check.
llvm::Value *Check::GetUniqtype(clang::QualType &QTy) {
  const clang::Type *Ty = QTy.getTypePtr();
  assert(Ty->isPointerType() && "Can't check non-pointer destination types");

  const clang::Type *ptrTy = Ty->getPointeeType().getTypePtr();

  std::cerr << "Converting type: ";
  ptrTy->dump();

  std::cout << getUniqtypeName(ptrTy) << std::endl;

  llvm::Module &TheModule = CGF.CGM.getModule();
  llvm::Type *utTy = llvm::Type::getInt8PtrTy(VMContext);
  llvm::Constant *ret = TheModule.getOrInsertGlobal("__uniqtype_long", utTy);
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
