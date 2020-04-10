#ifndef INTERPRET_H
#define INTERPRET_H

#include <list>
#include <stack>
#include "code.h"

class Interpreter {
  std::map<PRef, double> valueMap;
  std::stack<PRef> callStack;
public:
  Interpreter();
  double interpret(std::map<std::string, double> &context, const std::list<PRef> &code);
};

#endif
