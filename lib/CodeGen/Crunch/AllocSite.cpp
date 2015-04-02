#include <cstring>
#include <fstream>
#include <iostream>
#include <map>
#include <set>

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "Crunch/AllocFunction.h"
#include "Crunch/AllocSite.h"
#include "Crunch/Crunch.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"

namespace Crunch {

using clang::UnaryExprOrTypeTraitExpr;
using clang::RecursiveASTVisitor;
using std::ios;

// Visit sizeof expressions looking for their type.
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

    return false;
  }

  virtual bool VisitOffsetOfExpr(clang::OffsetOfExpr *E) {
    QType = E->getTypeSourceInfo()->getType();
    Found = true;
    return false;
  }

  bool typeFound() {
    return Found;
  }

  clang::QualType getType() {
    return QType;
  }
};

void AllocSite::getSourceLoc() {
  clang::SourceLocation Loc = Site->getExprLoc();
  clang::SourceManager &SM = CGF.getContext().getSourceManager();

  SourceFName = SM.getPresumedLoc(Loc).getFilename();
  SourceLine = SM.getPresumedLineNumber(Loc);

  SourceRealPath = realpath(SourceFName.c_str(), NULL);
}

static clang::Type *getVoidType() {
  static clang::BuiltinType *VoidType = nullptr;
  if (VoidType == nullptr) {
    VoidType = new clang::BuiltinType(clang::BuiltinType::Void);
  }
  return VoidType;
}

/* Recurse through CallExpr->getCallee() looking for the actual DeclRefExpr to
 * the callee. */
class DeclRefVisitor : public RecursiveASTVisitor<DeclRefVisitor> {
private:
  clang::DeclRefExpr *Ret = nullptr;

public:
  explicit DeclRefVisitor() {};
  virtual ~DeclRefVisitor() {};

  virtual bool VisitDeclRefExpr(clang::DeclRefExpr *E) {
    Ret = E;
    return false;
  }

  clang::DeclRefExpr *get() {
    return Ret;
  }
};

static clang::DeclRefExpr *findDeclRef(clang::Expr *E) {
  DeclRefVisitor Visitor;
  Visitor.TraverseStmt(E);
  auto Ret = Visitor.get();

  if (Ret == nullptr) {
    E->dump();
  }
  return Ret;
}

AllocSite::AllocSite(clang::CodeGen::CodeGenFunction &_CGF,
                     clang::CallExpr *_Site) :
                      CGF(_CGF), Site(_Site)
{
  valid = false;
  if (!CGF.SanOpts.has(clang::SanitizerKind::Crunch) &&
      !CGF.SanOpts.has(clang::SanitizerKind::Allocs)) {
    return;
  }

  auto Callee = findDeclRef(Site->getCallee());
  assert(Callee != nullptr && "No DeclRefExpr in CallExpr");

  auto Decl = Callee->getFoundDecl();
  FunName = Decl->getName().str();

  getSourceLoc();

  AllocFunction *AF = AllocFunction::get(FunName);
  if (AF == nullptr) {
    return;
  }

  unsigned NumArgs = Site->getNumArgs();
  assert(AF->getSizeArg() < NumArgs);
  clang::Expr *Arg = Site->getArg(AF->getSizeArg());

  AllocSizeVisitor Visitor;
  Visitor.TraverseStmt(Arg);

  if (!Visitor.typeFound()) {
    const clang::ASTContext &Context = CGF.getContext();
    clang::DiagnosticsEngine &diagEngine = Context.getDiagnostics();
    unsigned diagID = diagEngine.getCustomDiagID(
                        clang::DiagnosticsEngine::Warning,
                        "Could not infer type from allocation site");
    clang::SourceLocation Location = Site->getExprLoc();
    diagEngine.Report(Location, diagID);

    clang::QualType VoidType(getVoidType(), 0U);
    Type = VoidType;
    valid = true;
  } else {
    valid = true;
    Type = Visitor.getType();
  }
}

std::string AllocSite::getOutputFName() {
  std::string ExtRemoved = SourceFName.substr(0, SourceFName.length() - 2);
  return ExtRemoved + ".i.allocs";
}

std::ofstream *AllocSite::openOutputFile(void) {
  static std::map<std::string, std::ofstream *> OpenFiles;
  std::string FName = getOutputFName();

  // FIXME: Where do these get closed?
  if (OpenFiles.find(FName) == OpenFiles.end()) {
    OpenFiles[FName] = new std::ofstream(FName, ios::out | ios::trunc);
  }
  return OpenFiles[FName];
}

void AllocSite::emitIfValid(void) {
  if (!valid) {
    return;
  }

  std::ofstream &Out = *openOutputFile();
  Out << SourceRealPath << "\t" << SourceLine << "\t" << FunName << "\t";
  Out << getUniqtypeName(Type) << std::endl;
}

AllocSite::~AllocSite() {
  free(SourceRealPath);
}

} // namespace Crunch
