#include <assert.h>

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
#include <cstring>
#include <iostream>

#include "SanitizerCrunch.h"

using namespace clang;
using namespace CodeGen;

namespace Crunch {

static std::string getUniqtypeName(llvm::Type *Ty) {
  // Based on TypePrinting::print()
  switch (Ty->getTypeID()) {
  case llvm::Type::VoidTyID:    return "void";
  case llvm::Type::HalfTyID:    return "half";
  case llvm::Type::FloatTyID:   return "float";
  case llvm::Type::DoubleTyID:  return "double";

  case llvm::Type::IntegerTyID: {
    int width = (static_cast<llvm::IntegerType *>(Ty))->getBitWidth();
    /* Assume these integer types for now (since LLVM doesn't know about
     * different C int types. */
    switch (width) {
    case 8:   return "char";
    case 16:  return "short";
    case 32:  return "int";
    case 64:  return "long";
    }
  }

  case llvm::Type::PointerTyID:
    return "__PTR_" + getUniqtypeName(Ty->getPointerElementType());

  case llvm::Type::StructTyID:
    /* getStructName returns a value of the form "struct.foo", so remove the
     * first 7 characters ('struct.') */
    return Ty->getStructName().substr(7);

  case llvm::Type::FunctionTyID: {
    llvm::FunctionType *FTy = static_cast<llvm::FunctionType *>(Ty);
    std::string ret = "__FUN_FROM_";

    int param_num;
    for (auto it = FTy->param_begin();
         it != FTy->param_end(); ++FTy, ++param_num) {
      ret += "_ARG" + std::to_string(param_num) + "_";
      ret += getUniqtypeName(*it);
    }
    ret += "__FUN_TO_" + getUniqtypeName(FTy->getReturnType());
  }

  default:
    std::cerr << "unknown: ";
    Ty->dump();
  }

  return "<?>";
}

// GetUniqtype - return the correct uniqtype variable for a given type to
// check.
llvm::Value *Check::GetUniqtype(llvm::Type *Ty) {
  assert(DstTy->isPointerTy() && "Can't check non-pointer destination types");

  llvm::Type *ptrTy = DstTy->getPointerElementType();

  std::cerr << "Converting type: ";
  ptrTy->dump();

  std::cout << getUniqtypeName(ptrTy) << std::endl;

  llvm::Module &TheModule = CGF.CGM.getModule();
  llvm::Type *utTy = llvm::Type::getInt8PtrTy(VMContext);
  llvm::Constant *ret = TheModule.getOrInsertGlobal("__uniqtype_long", utTy);
  return Builder.CreateBitCast(ret, utTy);
}

void Check::Emit() {
  if (!CGF.SanOpts.has(SanitizerKind::Crunch))
    return;

  DstTy = CGF.ConvertType(DestClangTy);

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

  std::vector<llvm::Value *> ArgsV;
  ArgsV.push_back(Src);

  ArgsV.push_back(GetUniqtype(DstTy));

  Builder.CreateCall(CheckF, ArgsV, "crunchcheck");
}

void EmitCastCheck(CodeGenFunction &CGF, CGBuilderTy &Builder,
                   llvm::LLVMContext &VMContext,
                   llvm::Value *Src, clang::QualType &DestClangTy)
{
  Check c(CGF, Builder, VMContext, Src, DestClangTy);
  c.Emit();
}

} // namespace Crunch
