#include "interpret.h"
#include <cmath>
#include <type_traits>

Interpreter::Interpreter() {}

Interpreter::ValueType Interpreter::interpret(VarMap &context,
                                              const FuncMap &functions,
                                              const std::list<PRef> &code) {
  auto res = runInterpret(context, functions, code);
  valueMap.clear();
  return res;
}

template <typename Tp, typename Up>
inline constexpr bool is_t = std::is_same<Tp, Up>::value;

Interpreter::ValueType Interpreter::runInterpret(VarMap &context,
                                                 const FuncMap &functions,
                                                 const std::list<PRef> &code) {
  for (auto &i : code) {
    if (auto op = std::dynamic_pointer_cast<Code>(i)) {
      switch (op->type) {
      case COpType::IntToFloat:
        valueMap[i] = static_cast<double>(std::get<0>(valueMap[op->op1]));
        break;
      case COpType::Neg:
        valueMap[i] = -std::get<1>(valueMap[op->op1]);
        break;
      case COpType::Add:
        valueMap[i] =
            std::get<1>(valueMap[op->op1]) + std::get<1>(valueMap[op->op2]);
        break;
      case COpType::Sub:
        valueMap[i] =
            std::get<1>(valueMap[op->op1]) - std::get<1>(valueMap[op->op2]);
        break;
      case COpType::Div:
        valueMap[i] =
            std::get<1>(valueMap[op->op1]) / std::get<1>(valueMap[op->op2]);
        break;
      case COpType::Mul:
        valueMap[i] =
            std::get<1>(valueMap[op->op1]) * std::get<1>(valueMap[op->op2]);
        break;
      case COpType::IntNeg:
        valueMap[i] = -std::get<0>(valueMap[op->op1]);
        break;
      case COpType::IntAdd:
        valueMap[i] =
            std::get<0>(valueMap[op->op1]) + std::get<0>(valueMap[op->op2]);
        break;
      case COpType::IntSub:
        valueMap[i] =
            std::get<0>(valueMap[op->op1]) - std::get<0>(valueMap[op->op2]);
        break;
      case COpType::IntDiv:
        valueMap[i] =
            std::get<0>(valueMap[op->op1]) / std::get<0>(valueMap[op->op2]);
        break;
      case COpType::IntMul:
        valueMap[i] =
            std::get<0>(valueMap[op->op1]) * std::get<0>(valueMap[op->op2]);
        break;
      case COpType::Exp:
        valueMap[i] = std::pow(std::get<1>(valueMap[op->op1]),
                               std::get<1>(valueMap[op->op2]));
        break;
      case COpType::Assign: {
        auto val = valueMap[op->op2];
        valueMap[i] = val;
        valueMap[op->op1] = val;
        auto name = std::static_pointer_cast<NameRef>(op->op1)->nameRef;
        context[name] = val;
      } break;
      case COpType::Param:
        callStack.push(op->op1);
        break;
      case COpType::GetParam: {
        valueMap[i] = valueMap[callStack.top()];
        callStack.pop();
      } break;
      case COpType::Call: {
        auto name = std::static_pointer_cast<NameRef>(op->op1)->nameRef;
        if (name == "sin") {
          if (callStack.size() < 1) {
            throw std::runtime_error("Not enough arguments for sin(x)");
          }
          auto argPtr = callStack.top();
          callStack.pop();
          auto val = std::sin(std::get<1>(valueMap[argPtr]));
          valueMap[i] = val;
        } else if (name == "cos") {
          if (callStack.size() < 1) {
            throw std::runtime_error("Not enough arguments for cos(x)");
          }
          auto argPtr = callStack.top();
          callStack.pop();
          auto val = std::cos(std::get<1>(valueMap[argPtr]));
          valueMap[i] = val;
        } else {
          std::size_t call_arity =
              std::static_pointer_cast<IntRef>(op->op2)->val;
          auto functionCode = functions.find({name, call_arity});
          if (functionCode == functions.end()) {
            throw std::runtime_error("Unknown function " + name);
          }
          VarMap emptyContext;
          valueMap[i] =
              runInterpret(emptyContext, functions, functionCode->second);
        }
      } break;
      }
    } else if (auto val = std::dynamic_pointer_cast<ValRef>(i)) {
      valueMap[i] = val->valRef;
    } else if (auto val = std::dynamic_pointer_cast<IntRef>(i)) {
      valueMap[i] = val->val;
    } else if (auto name = std::dynamic_pointer_cast<NameRef>(i)) {
      valueMap[i] = context[name->nameRef];
    } else {
      throw std::runtime_error(std::string("Unknown node type ") +
                               typeid(i).name());
    }
  }
  return valueMap[code.back()];
}
