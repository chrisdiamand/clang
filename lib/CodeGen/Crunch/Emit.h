#ifndef LLVM_CLANG_LIB_CODEGEN_CRUNCH_EMIT_H
#define LLVM_CLANG_LIB_CODEGEN_CRUNCH_EMIT_H

#include "CodeGenFunction.h"
#include "CodeGenModule.h"

namespace Crunch {

void emitAssert(clang::CodeGen::CodeGenFunction &, llvm::Value *,
                const std::string &, const clang::SourceLocation &);

void emitIncrementCheckCount(clang::CodeGen::CodeGenFunction &);

} // namespace Crunch

#endif
