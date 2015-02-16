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
#include <cstdarg>

using namespace clang;
using namespace CodeGen;
using llvm::Value;

namespace Crunch {

// GetUniqtype - return the correct uniqtype variable for a given type to
// check.
static llvm::Value *GetUniqtype(CodeGenFunction &CGF, CGBuilderTy &Builder,
                                llvm::LLVMContext &VMContext, llvm::Type *Ty)
{
  llvm::Module &TheModule = CGF.CGM.getModule();
  llvm::Type *utTy = llvm::Type::getInt8PtrTy(VMContext);
  llvm::Constant *ret = TheModule.getOrInsertGlobal("__uniqtype_long", utTy);
  return Builder.CreateBitCast(ret, utTy);
}

// EmitCastCheck - Emit a call to libcrunch to check if the pointer really
// points to the the type we're casting it to.
void EmitCastCheck(CodeGenFunction &CGF, CGBuilderTy &Builder,
                   llvm::LLVMContext &VMContext,
                   Value *Src, llvm::Type *DstTy)
{
  if (!CGF.SanOpts.has(SanitizerKind::Crunch))
    return;

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

  std::vector<Value *> ArgsV;
  ArgsV.push_back(Src);

  ArgsV.push_back(GetUniqtype(CGF, Builder, VMContext, DstTy));

  Builder.CreateCall(CheckF, ArgsV, "crunchcheck");
}

} // namespace Crunch
