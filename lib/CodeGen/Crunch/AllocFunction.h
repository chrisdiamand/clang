#ifndef LLVM_CLANG_LIB_CODEGEN_CRUNCH_ALLOCFUNCTION_H
#define LLVM_CLANG_LIB_CODEGEN_CRUNCH_ALLOCFUNCTION_H

#include <map>
#include <string>

namespace Crunch {

class AllocFunction {
private:
  unsigned int SizeArgIndex;

  AllocFunction(int _SizeArgIndex) :
    SizeArgIndex(_SizeArgIndex) {};

  static std::map<std::string, AllocFunction *> Functions;
  static void add(std::string, int);
  static void addDefaults();
  static void ensureInitialized();

public:
  unsigned int getSizeArg();
  static AllocFunction *get(std::string);
};

} // namespace Crunch

#endif
