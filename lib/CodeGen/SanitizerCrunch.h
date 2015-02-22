#ifndef LLVM_CLANG_LIB_CODEGEN_SANITIZER_CRUNCH_H
#define LLVM_CLANG_LIB_CODEGEN_SANITIZER_CRUNCH_H

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

namespace Crunch {

// Which libcrunch function we need to call to check this type.
enum CheckFunctionKind {
  CT_Unknown,
  CT_IsA,
  CT_Named,
  CT_PointerOfDegree,
  CT_FunctionRefining
};

class Check {
private:
  clang::CodeGen::CodeGenFunction &CGF;
  clang::CodeGen::CGBuilderTy &Builder;
  llvm::LLVMContext &VMContext;
  llvm::Value *Src;

  inline llvm::Module &getModule() {
    return CGF.CGM.getModule();
  }

  clang::QualType &DestClangTy;
  clang::QualType PointeeTy;
  llvm::Type *DstTy;

  CheckFunctionKind CheckFunKind;
  std::string CrunchTypeName;

  llvm::Value *getUniqtypeVariable();
  llvm::Constant *getCheckFunction();

public:
  Check(clang::CodeGen::CodeGenFunction &CGF,
        clang::CodeGen::CGBuilderTy &Builder,
        llvm::LLVMContext &VMContext,
        llvm::Value *Src, clang::QualType &DestClangTy);
  void Emit();
};

void EmitCastCheck(clang::CodeGen::CodeGenFunction &CGF,
                   clang::CodeGen::CGBuilderTy &Builder,
                   llvm::LLVMContext &VMContext,
                   llvm::Value *Src, clang::QualType &DestClangTy);

} // namespace Crunch

#endif
