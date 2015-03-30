#include <assert.h>

#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Module.h"

#include <cstring>
#include <iostream>

#include "Crunch/AllocSite.h"
#include "Crunch/Check.h"
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
std::string parseType(const clang::QualType &Ty,
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

void emitCastCheck(CodeGenFunction &CGF, CGBuilderTy &Builder,
                   llvm::LLVMContext &VMContext, clang::Expr *ClangSrc,
                   llvm::Value *Src, clang::QualType &DestClangTy)
{
  Check c(CGF, Builder, VMContext, ClangSrc, Src, DestClangTy);
  c.emit();
}

void visitAllocSite(clang::CodeGen::CodeGenFunction &CGF,
                    const clang::CallExpr *Site)
{
  if (!CGF.SanOpts.has(clang::SanitizerKind::Crunch) &&
      !CGF.SanOpts.has(clang::SanitizerKind::Allocs)) {
    return;
  }

  AllocSite *AS = new AllocSite(CGF, const_cast<clang::CallExpr *>(Site));
  AS->emitIfValid();
  delete AS;
}

} // namespace Crunch
