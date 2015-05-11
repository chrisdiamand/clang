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
#include "Crunch/Emit.h"

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

  assert(false && "Invalid CheckFunKind");
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
    emitIncrementCheckCount(CGF);
  }

  // Cast the pointer to int8_t * to match __is_aU().
  Src = Builder.CreateBitCast(Src, llvm::Type::getInt8PtrTy(VMContext));

  std::vector<llvm::Value *> ArgsV;
  ArgsV.push_back(Src);

  llvm::Value *Uniqtype = getUniqtypeVariable();
  assert(Uniqtype != nullptr);
  llvm::Constant *CheckFun = getCheckFunction(Uniqtype->getType());

  ArgsV.push_back(Uniqtype);

  llvm::Value *CheckRet = Builder.CreateCall(CheckFun, ArgsV, "crunch_check");

  const std::string Msg = getCheckFunctionName(CheckFunKind)
                  + "(?, " + CrunchTypeName + ")";
  emitAssert(CGF, CheckRet, Msg, ClangSrc->getExprLoc());
}

} // namespace Crunch
