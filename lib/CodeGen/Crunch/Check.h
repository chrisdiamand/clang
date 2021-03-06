#ifndef LLVM_CLANG_LIB_CODEGEN_CRUNCH_CHECK_H
#define LLVM_CLANG_LIB_CODEGEN_CRUNCH_CHECK_H

#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include <cstring>

#include "Crunch.h"

namespace Crunch {

// Which libcrunch function we need to call to check this type.
enum CheckFunctionKind : unsigned int {
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
  const clang::Expr *ClangSrc;
  llvm::Value *Src;
  clang::QualType &DestClangTy;

  inline llvm::Module &getModule() {
    return CGF.CGM.getModule();
  }

  clang::QualType PointeeTy;

  CheckFunctionKind CheckFunKind;
  std::string CrunchTypeName;
  int PointerDegree;

  llvm::Value *getUniqtypeVariable();
  llvm::Constant *getCheckFunction(llvm::Type *);

public:
  Check(clang::CodeGen::CodeGenFunction &, const clang::Expr *,
        llvm::Value *, clang::QualType &);
  void emit();
};

bool sloppyFunctionPointers();

} // namespace Crunch

#endif
