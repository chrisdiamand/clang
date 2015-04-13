#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Module.h"

#include <vector>

#include "Crunch/Emit.h"

namespace Crunch {

/* Emit the 'false' case of the '__inline_assert' macro, i.e.:
 * __assert_fail(...); */
static void emitAssertFail(clang::CodeGen::CodeGenFunction &CGF,
                           const std::string &Message,
                           const clang::SourceLocation &Loc)
{
  auto &VMContext = CGF.getLLVMContext();
  auto &TheModule = CGF.CGM.getModule();

  std::vector<llvm::Value *> Args;
  Args.push_back(llvm::ConstantDataArray::getString(VMContext, Message));

  clang::SourceManager &SM = CGF.getContext().getSourceManager();
  Args.push_back(llvm::ConstantDataArray::getString(VMContext,
                                                    SM.getBufferName(Loc)));
  Args.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(VMContext),
                                        SM.getPresumedLineNumber(Loc)));
  Args.push_back(llvm::ConstantDataArray::getString(VMContext,
                                                    CGF.CurFn->getName()));

  std::vector<llvm::Type *> ArgTy;
  for (auto it = Args.begin(); it != Args.end(); ++it) {
    ArgTy.push_back((*it)->getType());
  }
  llvm::Type *RetTy = llvm::Type::getVoidTy(VMContext);
  llvm::ArrayRef<llvm::Type *> ArgTy_ar(ArgTy);

  llvm::FunctionType *FunTy = llvm::FunctionType::get(RetTy, ArgTy_ar, false);

  llvm::Constant *Fun = TheModule.getOrInsertFunction("__assert_fail", FunTy);
  CGF.Builder.CreateCall(Fun, Args);
}

/* Emit the functionality of the '__inline_assert' macro, i.e.:
 * if (!Pred) { __assert_fail(...); } */
void emitAssert(clang::CodeGen::CodeGenFunction &CGF, llvm::Value *Pred,
                const std::string &Message, const clang::SourceLocation &Loc)
{
  llvm::BasicBlock *StartBB, *BodyBB, *ExitBB;
  StartBB = CGF.Builder.GetInsertBlock();

  // Negate the condition by comparing to zero.
  Pred = CGF.Builder.CreateICmpEQ(Pred,
                                  llvm::ConstantInt::get(Pred->getType(), 0),
                                  "crunchAssert.cond");

  BodyBB = CGF.createBasicBlock("crunchAssert.body", CGF.CurFn);
  ExitBB = CGF.createBasicBlock("crunchAssert.exit");

  CGF.Builder.CreateCondBr(Pred, BodyBB, ExitBB);

  // Generate the 'if' body
  CGF.Builder.SetInsertPoint(BodyBB);
  emitAssertFail(CGF, Message, Loc);
  CGF.Builder.CreateBr(ExitBB);

  CGF.CurFn->getBasicBlockList().push_back(ExitBB);
  CGF.Builder.SetInsertPoint(ExitBB);
}

void emitIncrementCheckCount(clang::CodeGen::CodeGenFunction &CGF) {
  const llvm::StringRef CCName = "__libcrunch_begun";
  llvm::Type *CCType = llvm::Type::getInt32Ty(CGF.getLLVMContext());
  auto &TheModule = CGF.CGM.getModule();
  llvm::Constant *CheckCount = TheModule.getOrInsertGlobal(CCName, CCType);
  llvm::Constant *One = llvm::ConstantInt::get(CCType, 1);

  llvm::LoadInst *CCLoaded = CGF.Builder.CreateLoad(CheckCount);
  llvm::Value *CCAddOne = CGF.Builder.CreateAdd(CCLoaded, One, "CheckCount");
  CGF.Builder.CreateStore(CCAddOne, CheckCount);
}

} // namespace Crunch
