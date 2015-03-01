#include <cstring>
#include <fstream>
#include <iostream>
#include <map>
#include <set>

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "Crunch/Crunch.h"
#include "Crunch/Sites.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"

namespace Crunch {

using clang::UnaryExprOrTypeTraitExpr;
using clang::RecursiveASTVisitor;
using std::ios;

class AllocSizeVisitor : public RecursiveASTVisitor<AllocSizeVisitor> {
private:
  clang::QualType   QType;
  bool              Found;

public:
  explicit AllocSizeVisitor() {
    Found = false;
  }

  virtual ~AllocSizeVisitor() {};

  virtual bool VisitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *E) {
    auto Kind = E->getKind();
    if (Kind != clang::UETT_SizeOf) {
      return true;
    }

    QType = E->getArgumentType();
    Found = true;

    return true;
  }

  bool typeFound() {
    return Found;
  }

  clang::QualType getType() {
    return QType;
  }
};

struct AllocFunction {
  unsigned SizeArgIndex;

  AllocFunction(int _SizeArgIndex) :
    SizeArgIndex(_SizeArgIndex) {};
};

static std::map<std::string, AllocFunction *> AllocFunctions;

static void registerAllocFunction(std::string Name, int SizeArgIndex) {
  AllocFunction *AF = new AllocFunction(SizeArgIndex);
  AllocFunctions[Name] = AF;
}

static void registerAllocFunctions(void) {
  if (AllocFunctions.size() != 0) {
    return;
  }
  registerAllocFunction("malloc", 0);
  registerAllocFunction("calloc", 1);
  registerAllocFunction("realloc", 1);
}

static AllocFunction *getAllocFunction(std::string Name) {
  registerAllocFunctions();
  auto it = AllocFunctions.find(Name);

  if (it == AllocFunctions.end()) {
    return nullptr;
  }
  return it->second;
}

void AllocSite::getSourceLoc() {
  clang::SourceLocation Loc = Site->getExprLoc();
  clang::SourceManager &SM = CGF.getContext().getSourceManager();
  SourceFName = SM.getBufferName(Loc);
  SourceLine = SM.getPresumedLineNumber(Loc);

  SourceRealPath = realpath(SourceFName.c_str(), NULL);
}

AllocSite::AllocSite(clang::CodeGen::CodeGenFunction &_CGF,
                     clang::CallExpr *_Site) :
                      CGF(_CGF), Site(_Site)
{
  if (!CGF.SanOpts.has(clang::SanitizerKind::Crunch)) {
    valid = false;
    return;
  }

  auto Callee = Site->getCallee()->IgnoreImplicit()->IgnoreParens();
  auto CalleeDR = clang::dyn_cast<clang::DeclRefExpr>(Callee);
  FunName = CalleeDR->getFoundDecl()->getName().str();

  getSourceLoc();

  AllocFunction *AF = getAllocFunction(FunName);
  if (AF == nullptr) {
    valid = false;
    return;
  }

  unsigned NumArgs = Site->getNumArgs();
  assert(AF->SizeArgIndex < NumArgs);
  clang::Expr *Arg = static_cast<clang::Expr *>(Site->getArg(AF->SizeArgIndex));

  AllocSizeVisitor *Visitor = new AllocSizeVisitor();
  Visitor->TraverseStmt(Arg);

  if (!Visitor->typeFound()) {
    const clang::ASTContext &Context = CGF.getContext();
    clang::DiagnosticsEngine &diagEngine = Context.getDiagnostics();
    unsigned diagID = diagEngine.getCustomDiagID(
                        clang::DiagnosticsEngine::Warning,
                        "Could not infer type from allocation site");
    clang::SourceLocation Location = Site->getExprLoc();
    diagEngine.Report(Location, diagID);

    valid = false;
  } else {
    valid = true;
    Type = Visitor->getType();
  }
  delete Visitor;
}

std::string AllocSite::getOutputFName() {
  std::string ExtRemoved = SourceFName.substr(0, SourceFName.length() - 2);
  return ExtRemoved + ".i.allocs";
}

void AllocSite::emitIfValid(void) {
  if (!valid) {
    return;
  }

  std::string FName = getOutputFName();
  std::cout << FName << "!!!" << std::endl;

  std::ofstream Out(getOutputFName(), ios::out | ios::app);
  Out << SourceRealPath << "\t" << SourceLine << "\t" << FunName << "\t";
  Out << getUniqtypeName(Type) << std::endl;
  Out.close();
}

AllocSite::~AllocSite() {
  free(SourceRealPath);
}

void visitAllocSite(clang::CodeGen::CodeGenFunction &CGF,
                    clang::CallExpr *Site)
{
  AllocSite *AS = new AllocSite(CGF, Site);
  AS->emitIfValid();
  delete AS;
}

} // namespace Crunch
