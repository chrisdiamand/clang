#include <assert.h>

#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Module.h"

#include <cstring>
#include <iostream>

#include "Crunch/Check.h"
#include "Crunch/Crunch.h"

using namespace clang;
using namespace CodeGen;

namespace Crunch {

// GetUniqtype - return the correct uniqtype variable for a given type to
// check.
llvm::Value *Check::getUniqtypeVariable() {
  switch (CheckFunKind) {
    case CT_NoCheck:
      return nullptr;

    case CT_IsA:
    case CT_FunctionRefining: {
      std::string UniqtypeName = "__uniqtype__" + CrunchTypeName;

      llvm::Type *utTy = llvm::Type::getInt8PtrTy(VMContext);
      llvm::Constant *ret = getModule().getOrInsertGlobal(UniqtypeName, utTy);
      return Builder.CreateBitCast(ret, utTy);
    }

    case CT_Named:
      return llvm::ConstantDataArray::getString(VMContext, CrunchTypeName);

    case CT_PointerOfDegree:
      return llvm::ConstantInt::get(llvm::Type::getInt32Ty(VMContext),
                                    PointerDegree);
  }

  return nullptr;
}

Check::Check(clang::CodeGen::CodeGenFunction &_CGF, const clang::Expr *_ClSrc,
             llvm::Value *_Src, clang::QualType &_DestClangTy) :
  CGF(_CGF), Builder(_CGF.Builder), VMContext(_CGF.getLLVMContext()),
  ClangSrc(_ClSrc), Src(_Src), DestClangTy(_DestClangTy)
{
  assert(DestClangTy->isPointerType() && "Can't check non-pointer destination types");
  PointeeTy = DestClangTy->getPointeeType();
  /* Need to strip parentheses; these occur around function prototypes and mean
   * that the QualType can't be casted directly to a FunctionProtoType. */
  PointeeTy = PointeeTy.IgnoreParens();

  PointerDegree = 0;
  CrunchTypeName = parseType(PointeeTy, &CheckFunKind, &PointerDegree);
}

bool sloppyFunctionPointers() {
  if (getenv("LIBCRUNCH_SLOPPY_FUNCTION_POINTERS") != nullptr) {
    return true;
  }
  return false;
}

static std::string getCheckFunctionName(CheckFunctionKind Kind) {
  switch (Kind) {
    case CT_NoCheck:            return "__no_check";
    case CT_IsA:                return "__is_aU_not_inlined";
    case CT_Named:              return "__named_a_internal";
    case CT_PointerOfDegree:    return "__is_a_pointer_of_degree_internal";
    case CT_FunctionRefining:   return "__is_a_function_refining_internal";
  }
  assert(false && "Invalid CheckFunctionKind");
  return "ERROR";
}

llvm::Constant *Check::getCheckFunction(llvm::Type *SecondArg) {
  llvm::Constant *Ret = nullptr;
  llvm::Type *ArgTy[2];
  llvm::Module &TheModule = getModule();
  llvm::Type *ResTy = llvm::Type::getInt32Ty(VMContext);
  std::string FunName = getCheckFunctionName(CheckFunKind);

  // The first argument is always the pointer to be checked.
  ArgTy[0] = llvm::Type::getInt8PtrTy(VMContext);
  ArgTy[1] = SecondArg;

  auto ArgTyPtr = const_cast<llvm::Type **>(ArgTy);
  llvm::ArrayRef<llvm::Type *> ArgTy_ar(ArgTyPtr, 2);
  llvm::FunctionType *FunTy = llvm::FunctionType::get(ResTy, ArgTy_ar, false);
  Ret = TheModule.getOrInsertFunction(FunName, FunTy);

  return Ret;
}

/* Emit the functionality of the '__inline_assert' macro, i.e.:
 * if (!Pred) { __assert_fail(...); } */
void Check::emitAssert(llvm::Value *Pred) {
  llvm::BasicBlock *StartBB, *BodyBB, *ExitBB;
  StartBB = Builder.GetInsertBlock();

  // Negate the condition by comparing to zero.
  Pred = Builder.CreateICmpEQ(Pred,
                              llvm::ConstantInt::get(Pred->getType(), 0),
                              "crunchAssert.cond");

  BodyBB = CGF.createBasicBlock("crunchAssert.body", CGF.CurFn);
  ExitBB = CGF.createBasicBlock("crunchAssert.exit");

  Builder.CreateCondBr(Pred, BodyBB, ExitBB);

  // Generate the 'if' body
  Builder.SetInsertPoint(BodyBB);
  emitAssertFail();
  Builder.CreateBr(ExitBB);

  CGF.CurFn->getBasicBlockList().push_back(ExitBB);
  Builder.SetInsertPoint(ExitBB);
}

/* Emit the 'false' case of the '__inline_assert' macro, i.e.:
 * __assert_fail(...); */
void Check::emitAssertFail() {
  std::string Message = getCheckFunctionName(CheckFunKind)
                      + "(?, " + CrunchTypeName + ")";

  llvm::Value *Args[4];
  Args[0] = llvm::ConstantDataArray::getString(VMContext, Message);

  clang::SourceLocation Loc = ClangSrc->getExprLoc();
  clang::SourceManager &SM = CGF.getContext().getSourceManager();
  Args[1] = llvm::ConstantDataArray::getString(VMContext,
                                               SM.getBufferName(Loc));
  Args[2] = llvm::ConstantInt::get(llvm::Type::getInt32Ty(VMContext),
                                   SM.getPresumedLineNumber(Loc));
  Args[3] = llvm::ConstantDataArray::getString(VMContext,
                                               CGF.CurFn->getName());

  llvm::Type *ArgTy[4];
  for (unsigned int i = 0; i < 5; ++i) {
    ArgTy[i] = Args[i]->getType();
  }
  llvm::Type *ResTy = llvm::Type::getVoidTy(VMContext);

  llvm::ArrayRef<llvm::Type *> ArgTy_ar(const_cast<llvm::Type **>(ArgTy), 4);
  llvm::FunctionType *FunTy = llvm::FunctionType::get(ResTy, ArgTy_ar, false);

  llvm::Constant *AssertFun = getModule().getOrInsertFunction("__assert_fail",
                                                              FunTy);
  Builder.CreateCall(AssertFun, Args);
}

void Check::emitIncrementCheckCount() {
  const StringRef CCName = "__libcrunch_begun";
  llvm::Type *CCType = llvm::Type::getInt32Ty(VMContext);
  llvm::Constant *CheckCount = getModule().getOrInsertGlobal(CCName, CCType);
  llvm::Constant *One = llvm::ConstantInt::get(CCType, 1);

  llvm::LoadInst *CCLoaded = Builder.CreateLoad(CheckCount);
  llvm::Value *CCAddOne = Builder.CreateAdd(CCLoaded, One, "CheckCount");
  Builder.CreateStore(CCAddOne, CheckCount);
}

void Check::emit() {
  if (!CGF.SanOpts.has(SanitizerKind::Crunch) || CheckFunKind == CT_NoCheck)
    return;

  /* When sloppyFunctionPointers is enabled, we check the actual function call
   * instead of the function pointer cast. */
  if (sloppyFunctionPointers() && CheckFunKind == CT_FunctionRefining)
    return;

  /* The IsA check calls a function which already increments the counter for
   * us. The other calls just skip straight to calling %_internal. */
  if (CheckFunKind != CT_IsA) {
    emitIncrementCheckCount();
  }

  // Cast the pointer to int8_t * to match __is_aU().
  Src = Builder.CreateBitCast(Src, llvm::Type::getInt8PtrTy(VMContext));

  std::vector<llvm::Value *> ArgsV;
  ArgsV.push_back(Src);

  llvm::Value *Uniqtype = getUniqtypeVariable();
  llvm::Constant *CheckFun = getCheckFunction(Uniqtype->getType());

  ArgsV.push_back(Uniqtype);

  llvm::Value *CheckRet = Builder.CreateCall(CheckFun, ArgsV, "crunch_check");

  emitAssert(CheckRet);
}

} // namespace Crunch
