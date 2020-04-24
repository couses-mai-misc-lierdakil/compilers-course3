#include "typeCheck.h"
#include <cassert>

void TypeCheck::visit(VarTypes &localVariableTypes, const PExpr &node) {
  if (node->visitedTc == Expr::VisitState::PermMark)
    return;
  else if (node->visitedTc == Expr::VisitState::TempMark) {
    throw std::runtime_error("Cycle detected");
  }
  node->visitedTc = Expr::VisitState::TempMark;
  if (auto binOp = std::dynamic_pointer_cast<BinOp>(node)) {
    visit(localVariableTypes, binOp->op1);
    visit(localVariableTypes, binOp->op2);
    node->visitedTc = Expr::VisitState::PermMark;
  } else if (auto neg = std::dynamic_pointer_cast<Negate>(node)) {
    visit(localVariableTypes, neg->op);
    node->type = neg->op->type;
    node->visitedTc = Expr::VisitState::PermMark;
  } else if (auto val = std::dynamic_pointer_cast<Value>(node)) {
    node->visitedTc = Expr::VisitState::PermMark;
    node->type = Type::FloatType;
  } else if (auto name = std::dynamic_pointer_cast<Constant>(node)) {
    node->visitedTc = Expr::VisitState::PermMark;
    auto varType = localVariableTypes.find(name->name);
    if (varType == localVariableTypes.end()) {
      throw std::runtime_error("Unknown variable " + name->name);
    }
    node->type = varType->second;
  } else if (auto assign = std::dynamic_pointer_cast<Assignment>(node)) {
    visit(localVariableTypes, assign->value);
    node->visitedTc = Expr::VisitState::PermMark;
    node->type = assign->type;
    localVariableTypes[assign->varName] = node->type;
  } else if (auto func = std::dynamic_pointer_cast<FunctionCall>(node)) {
    visit(localVariableTypes, func->args);
    auto name = func->name;
    node->visitedTc = Expr::VisitState::PermMark;
    if (name == "sin" || name == "cos") {
      assert(func->args->args.size() == 1);
      node->type = Type::FloatType;
      func->argTypes = {Type::FloatType};
    } else {
      std::size_t call_arity = func->arity();
      auto functionCode = functions.find({name, call_arity});
      if (functionCode == functions.end()) {
        throw std::runtime_error("Unknown function " + name + " with arity " +
                                 std::to_string(call_arity));
      }
      auto t = functionReturnTypes.at({name, func->arity()});
      func->argTypes = t.second;
      int argIdx = 0;
      for (auto &arg : func->args->args) {
        auto callArgType = arg->type;
        auto defArgType = func->argTypes[argIdx];
        if (callArgType == defArgType)
          continue;
        else if (callArgType == Type::IntType && defArgType == Type::FloatType)
          continue;
        else if (callArgType == Type::FloatType &&
                 defArgType == Type::IntType) {
          throw std::runtime_error("Typecheck failed on call to " + func->name +
                                   " argument " + std::to_string(argIdx + 1) +
                                   " has incorrect type");
        }
        argIdx++;
      }
      node->type = t.first;
    }
  } else if (auto args = std::dynamic_pointer_cast<Arguments>(node)) {
    for (auto &a : args->args) {
      visit(localVariableTypes, a);
    }
  } else if (auto intVal = std::dynamic_pointer_cast<IntValue>(node)) {
    node->visitedTc = Expr::VisitState::PermMark;
    node->type = Type::IntType;
  } else if (auto def = std::dynamic_pointer_cast<FunctionDef>(node)) {
    VarTypes newVariableTypes;
    for (auto &arg : def->def->args) {
      newVariableTypes[arg.first] = arg.second;
    }
    visit(newVariableTypes, def->expr);
    functionReturnTypes[{def->name, def->arity()}] = {def->type, def->argTypes};
  } else {
    throw std::runtime_error(std::string("Unknown node type ") +
                             typeid(node).name());
  }
}

void TypeCheck::typecheck(const PExpr &node) { visit(variableTypes, node); }
