#ifndef LLVM_CLANG_LIB_CODEGEN_CRUNCH_CRUNCH_H
#define LLVM_CLANG_LIB_CODEGEN_CRUNCH_CRUNCH_H

#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include <cstring>

namespace Crunch {

enum CheckFunctionKind : unsigned int;

void emitCastCheck(clang::CodeGen::CodeGenFunction &CGF, clang::Expr *ClangSrc,
                   llvm::Value *Src, clang::QualType &DestClangTy);

void visitAllocSite(clang::CodeGen::CodeGenFunction &,
                    const clang::CallExpr *);

std::string parseType(const clang::QualType &,
                      CheckFunctionKind *, int *);
std::string getUniqtypeName(const clang::QualType &);

} // namespace Crunch

#endif
