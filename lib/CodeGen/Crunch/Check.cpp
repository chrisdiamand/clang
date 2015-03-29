#include <assert.h>

#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Module.h"

#include <cstring>
#include <iostream>

#include "Crunch/Crunch.h"

using namespace clang;
using namespace CodeGen;

namespace Crunch {

/* Recurse down the type structure, returning the string used by libcrunch to
 * represent that type, and finding out which libcrunch function needs to be
 * called to check it. */
static std::string parseType_actual(const clang::QualType &Ty,
                                    CheckFunctionKind *CheckFunResult,
                                    int *PointerDegree)
{
  clang::LangOptions langOpts;
  clang::PrintingPolicy printPol(langOpts);
  CheckFunctionKind CheckFunKind = CT_IsA;
  std::string Ret = "__UNKNOWN_TYPE__";

  // Based on TypePrinting::print()
  if (Ty->isBuiltinType()) {
    auto BTy = clang::cast<clang::BuiltinType>(Ty);
    if (BTy->isVoidType()) {
      CheckFunKind = CT_PointerOfDegree;
    } else {
      CheckFunKind = CT_IsA;
    }
    Ret = BTy->getName(printPol).str();

    /* HACK: Libcrunch doesn't abbreviate 'short int' to 'short' (and so on),
     * so we need to add it in. */
    if (BTy->isIntegerType()) {
      if (Ret != "int") {
        Ret = Ret + "_int";
      }
    }

  } else if (Ty->isRecordType()) {
    auto RTy = Ty->getAsStructureType();
    Ret = RTy->getDecl()->getName().str();
    CheckFunKind = CT_Named;

  } else if (Ty->isPointerType()) {
    clang::QualType PtrTy = Ty->getPointeeType();
    Ret = "__PTR_" + parseType_actual(PtrTy, &CheckFunKind, nullptr);

    if (PointerDegree != nullptr) {
      *PointerDegree += 1;
    }

  } else if (Ty->isFunctionProtoType()) {
    auto FTy = clang::cast<const clang::FunctionProtoType>(Ty);
    Ret = "__FUN_FROM_";

    int NumParams = FTy->getNumParams();
    for (int i = 0; i < NumParams; ++i) {
      Ret += "__ARG" + std::to_string(i) + "_";
      Ret += parseType_actual(FTy->getParamType(i), nullptr, nullptr);
    }

    auto ReturnType = FTy->getReturnType();
    Ret += "__FUN_TO_" + parseType_actual(ReturnType, nullptr, nullptr);
    CheckFunKind = CT_FunctionRefining;

  } else {
    std::cerr << "Unknown type class: ";
    Ty->dump();
  }

  if (CheckFunResult != nullptr) {
    *CheckFunResult = CheckFunKind;
  }

  return Ret;
}

// Wrapper to skip checks to 'void *' and 'char *'.
static std::string parseType(const clang::QualType &Ty,
                             CheckFunctionKind *CheckFunResult,
                             int *PointerDegree)
{
  if (PointerDegree) {
    *PointerDegree = 0;
  }

  if (CheckFunResult) {
    *CheckFunResult = CT_NoCheck;
  }

  if (!Ty->isVoidType() && !Ty->isCharType()) {
    return parseType_actual(Ty, CheckFunResult, PointerDegree);
  }

  return "ERROR";
}

std::string getUniqtypeName(const clang::QualType &Ty) {
  return "__uniqtype__" + parseType_actual(Ty, nullptr, nullptr);
}

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

Check::Check(clang::CodeGen::CodeGenFunction &_CGF,
             clang::CodeGen::CGBuilderTy &_Builder,
             llvm::LLVMContext &_VMContext, clang::Expr *_ClangSrc,
             llvm::Value *_Src, clang::QualType &_DestClangTy) :
  CGF(_CGF), Builder(_Builder), VMContext(_VMContext),
  ClangSrc(_ClangSrc), Src(_Src), DestClangTy(_DestClangTy)
{
  assert(DestClangTy->isPointerType() && "Can't check non-pointer destination types");
  PointeeTy = DestClangTy->getPointeeType();
  /* Need to strip parentheses; these occur around function prototypes and mean
   * that the QualType can't be casted directly to a FunctionProtoType. */
  PointeeTy = PointeeTy.IgnoreParens();

  PointerDegree = 0;
  CrunchTypeName = parseType(PointeeTy, &CheckFunKind, &PointerDegree);
}

static std::string getCheckFunctionName(CheckFunctionKind Kind) {
  switch (Kind) {
    case CT_NoCheck:            return "__no_check";
    case CT_IsA:                return "__is_a_internal";
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

  emitIncrementCheckCount();

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

void emitCastCheck(CodeGenFunction &CGF, CGBuilderTy &Builder,
                   llvm::LLVMContext &VMContext, clang::Expr *ClangSrc,
                   llvm::Value *Src, clang::QualType &DestClangTy)
{
  Check c(CGF, Builder, VMContext, ClangSrc, Src, DestClangTy);
  c.emit();
}

} // namespace Crunch
