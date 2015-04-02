#include <stdio.h>
#include <string>

#include "Crunch/AllocFunction.h"

namespace Crunch {

std::map<std::string, AllocFunction *> AllocFunction::Functions;

void AllocFunction::add(std::string Name, int SizeArgIndex) {
  AllocFunction *AF = new AllocFunction(SizeArgIndex);
  Functions[Name] = AF;
}

void AllocFunction::addDefaults(void) {
  add("__builtin_alloca", 0);
  add("alloca", 0);
  add("calloc", 1);
  add("malloc", 0);
  add("realloc", 1);
}

void AllocFunction::ensureInitialized() {
  if (Functions.size() == 0) {
    addDefaults();
  }
}

AllocFunction *AllocFunction::get(std::string Name) {
  ensureInitialized();

  auto it = Functions.find(Name);

  if (it == Functions.end()) {
    return nullptr;
  }
  return it->second;
}

unsigned int AllocFunction::getSizeArg() {
  return SizeArgIndex;
}

} // namespace Crunch
