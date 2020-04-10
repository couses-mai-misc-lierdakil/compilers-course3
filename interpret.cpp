#include "interpret.h"
#include <cmath>

Interpreter::Interpreter() {}

double Interpreter::interpret(std::map<std::string, double> &context,
                              const std::list<PRef> &code) {
  valueMap.clear();
  for (auto &i : code) {
    if (auto op = std::dynamic_pointer_cast<Code>(i)) {
      switch (op->type) {
      case COpType::Neg:
        valueMap[i] = -valueMap[op->op1];
        break;
      case COpType::Add:
        valueMap[i] = valueMap[op->op1] + valueMap[op->op2];
        break;
      case COpType::Sub:
        valueMap[i] = valueMap[op->op1] - valueMap[op->op2];
        break;
      case COpType::Div:
        valueMap[i] = valueMap[op->op1] / valueMap[op->op2];
        break;
      case COpType::Mul:
        valueMap[i] = valueMap[op->op1] * valueMap[op->op2];
        break;
      case COpType::Exp:
        valueMap[i] = std::pow(valueMap[op->op1], valueMap[op->op2]);
        break;
      case COpType::Assign: {
        auto val = valueMap[op->op2];
        valueMap[i] = val;
        valueMap[op->op1] = val;
        context[std::static_pointer_cast<NameRef>(op->op1)->nameRef] = val;
      } break;
      case COpType::Param:
        callStack.push(op->op1);
        break;
      case COpType::Call: {
        auto name = std::static_pointer_cast<NameRef>(op->op1)->nameRef;
        if (name == "sin") {
          if (callStack.size() < 1) {
            throw std::runtime_error("Not enough arguments for sin(x)");
          }
          auto argPtr = callStack.top();
          callStack.pop();
          auto val = std::sin(valueMap[argPtr]);
          valueMap[i] = val;
        } else if (name == "cos") {
          if (callStack.size() < 1) {
            throw std::runtime_error("Not enough arguments for cos(x)");
          }
          auto argPtr = callStack.top();
          callStack.pop();
          auto val = std::cos(valueMap[argPtr]);
          valueMap[i] = val;
        } else {
          throw std::runtime_error("Unknown function " + name);
        }
      } break;
      }
    } else if (auto val = std::dynamic_pointer_cast<ValRef>(i)) {
      valueMap[i] = val->valRef;
    } else if (auto name = std::dynamic_pointer_cast<NameRef>(i)) {
      valueMap[i] = context[name->nameRef];
    } else {
      throw std::runtime_error(std::string("Unknown node type ") +
                               typeid(i).name());
    }
  }
  return valueMap[code.back()];
}
