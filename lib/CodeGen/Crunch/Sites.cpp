#include <iostream>
#include <map>
#include <stdio.h>

#include "clang/AST/RecursiveASTVisitor.h"
#include "Crunch/Crunch.h"

namespace Crunch {

using clang::UnaryExprOrTypeTraitExpr;
using clang::RecursiveASTVisitor;

class AllocSizeVisitor : public RecursiveASTVisitor<AllocSizeVisitor> {
public:
  explicit AllocSizeVisitor() {};
  virtual ~AllocSizeVisitor() {};

  virtual bool VisitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *E) {
    fprintf(stderr, "Hi! ");
    E->dump();
    return true;
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

void visitAllocSite(clang::CodeGen::CodeGenFunction &CGF,
                    clang::CallExpr *E)
{
  if (!CGF.SanOpts.has(clang::SanitizerKind::Crunch))
    return;

  printf("Function call!\n");
  auto Callee = E->getCallee()->IgnoreImplicit()->IgnoreParens();
  auto CalleeDR = clang::dyn_cast<clang::DeclRefExpr>(Callee);
  std::string FunName = CalleeDR->getFoundDecl()->getName().str();

  AllocFunction *AF = getAllocFunction(FunName);
  if (AF == nullptr) {
    printf("Not an alloc site\n");
    return;
  }

  unsigned NumArgs = E->getNumArgs();
  assert(AF->SizeArgIndex < NumArgs);
  clang::Expr *Arg = static_cast<clang::Expr *>(E->getArg(AF->SizeArgIndex));
  Arg->dump();

  AllocSizeVisitor *Visitor = new AllocSizeVisitor();
  Visitor->TraverseStmt(Arg);
  delete Visitor;
}

} // namespace Crunch
