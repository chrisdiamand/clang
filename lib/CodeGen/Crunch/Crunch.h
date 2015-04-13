#ifndef LLVM_CLANG_LIB_CODEGEN_CRUNCH_CRUNCH_H
#define LLVM_CLANG_LIB_CODEGEN_CRUNCH_CRUNCH_H

#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include <cstring>

namespace Crunch {

enum CheckFunctionKind : unsigned int;

void emitCastCheck(clang::CodeGen::CodeGenFunction &, const clang::Expr *,
                   llvm::Value *, clang::QualType &);

void checkCallArgs(clang::CodeGen::CodeGenFunction &, const clang::CallExpr *,
                   llvm::Value *, clang::CodeGen::CallArgList &);

void checkCallRet(clang::CodeGen::CodeGenFunction &,
                  const clang::CallExpr *, llvm::Value *);

llvm::Value *markSizeofExpr(clang::CodeGen::CodeGenFunction &,
                            const clang::Expr *, llvm::Value *);

std::string parseType(const clang::QualType &,
                      CheckFunctionKind *, int *);

std::string getUniqtypeName(const clang::QualType &);

extern inline bool isEnabled(clang::CodeGen::CodeGenFunction &CGF) {
  return CGF.SanOpts.has(clang::SanitizerKind::Allocs) ||
         CGF.SanOpts.has(clang::SanitizerKind::Crunch);
}

} // namespace Crunch

#endif
