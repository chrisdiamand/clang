#ifndef LLVM_CLANG_LIB_CODEGEN_SANITIZER_CRUNCH_H
#define LLVM_CLANG_LIB_CODEGEN_SANITIZER_CRUNCH_H

#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include <cstring>

namespace Crunch {

// Which libcrunch function we need to call to check this type.
enum CheckFunctionKind {
  CT_NoCheck,
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
  clang::Expr *ClangSrc;
  llvm::Value *Src;
  clang::QualType &DestClangTy;

  inline llvm::Module &getModule() {
    return CGF.CGM.getModule();
  }

  clang::QualType PointeeTy;
  llvm::Type *DstTy;

  CheckFunctionKind CheckFunKind;
  std::string CrunchTypeName;
  int PointerDegree;

  llvm::Value *getUniqtypeVariable();
  llvm::Constant *getCheckFunction(llvm::Type *);
  void emitAssert(llvm::Value *);

public:
  Check(clang::CodeGen::CodeGenFunction &_CGF,
        clang::CodeGen::CGBuilderTy &_Builder,
        llvm::LLVMContext &_VMContext, clang::Expr *_ClangSrc,
        llvm::Value *_Src, clang::QualType &_DestClangTy);
  void emit();
};

void emitCastCheck(clang::CodeGen::CodeGenFunction &CGF,
                   clang::CodeGen::CGBuilderTy &Builder,
                   llvm::LLVMContext &VMContext, clang::Expr *ClangSrc,
                   llvm::Value *Src, clang::QualType &DestClangTy);

} // namespace Crunch

#endif