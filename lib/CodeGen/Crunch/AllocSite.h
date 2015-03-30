#ifndef LLVM_CLANG_LIB_CODEGEN_CRUNCH_SITES_H
#define LLVM_CLANG_LIB_CODEGEN_CRUNCH_SITES_H

#include <cstring>
#include <fstream>

#include "CodeGenFunction.h"
#include "CodeGenModule.h"

namespace Crunch {

class AllocSite {
private:
    clang::CodeGen::CodeGenFunction     &CGF;
    clang::CallExpr                     *Site;
    std::string                         FunName;
    clang::QualType                     Type;
    bool                                valid;

    std::string                         SourceFName;
    unsigned int                        SourceLine;
    char                                *SourceRealPath;

    std::string getOutputFName(void);
    void getSourceLoc(void);
    std::ofstream *openOutputFile(void);

public:
    AllocSite(clang::CodeGen::CodeGenFunction &, clang::CallExpr *);
    ~AllocSite();
    void emitIfValid(void);
};

} // namespace Crunch

#endif
