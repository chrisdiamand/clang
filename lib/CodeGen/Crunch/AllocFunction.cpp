#include <assert.h>
#include <iostream>
#include <regex>
#include <stdio.h>
#include <string>

#include "Crunch/AllocFunction.h"

namespace Crunch {

std::map<std::string, AllocFunction *> AllocFunction::Functions;

// Parse a libcrunch-style allocation function description.
void AllocFunction::parseDescr(const std::string &Descr) {
  std::smatch sm;
  const std::regex Expr("([a-zA-Z_][a-zA-Z0-9_]*)\\(([a-zA-Z]*)\\)([a-zA-Z])");
  std::regex_match(Descr, sm, Expr);
  if (sm.size() == 0) {
    std::cerr << "Error: '" << Descr
              << "' not a valid allocation function descriptor.\n";
    valid = false;
    return;
  }

  assert(sm.size() == 4);

  Name = sm[1];
  Args = sm[2];
  Return = sm[3];

  SizeArgIndex = 0;
  for (size_t i = 0; i < Args.size(); ++i) {
    if (Args.at(i) == 'Z') {
      SizeArgIndex = i;
    }
  }
}

AllocFunction::AllocFunction(const std::string &Descr) {
  valid = true;
  parseDescr(Descr);
}

void AllocFunction::add(const std::string &Descr) {
  AllocFunction *AF = new AllocFunction(Descr);
  if (AF->valid) {
    Functions[AF->Name] = AF;
  } else {
    delete AF;
  }
}

void AllocFunction::addDefaults(void) {
  add("alloca(Z)p");
  add("malloc(Z)p");
  add("calloc(zZ)p");
  add("realloc(pZ)p");
  add("memalign(zZ)p");
}

void AllocFunction::ensureInitialized() {
  if (Functions.size() == 0) {
    addDefaults();
  }
}

AllocFunction *AllocFunction::get(const std::string &Name) {
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
