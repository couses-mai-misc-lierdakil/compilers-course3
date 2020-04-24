#ifndef INTERPRET_H
#define INTERPRET_H

#include "code.h"
#include <list>
#include <stack>
#include <variant>

class Interpreter {
public:
  using ValueType = std::variant<int, double>;

private:
  std::map<PRef, ValueType> valueMap;
  std::stack<PRef> callStack;

public:
  using VarMap = std::map<std::string, ValueType>;
  using FuncMap =
      std::map<std::pair<std::string, std::size_t>, std::list<PRef>>;
  Interpreter();
  ValueType interpret(VarMap &context, const FuncMap &functions,
                      const std::list<PRef> &code);

private:
  ValueType runInterpret(VarMap &context, const FuncMap &functions,
                         const std::list<PRef> &code);
};

#endif
