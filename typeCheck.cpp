#include "typeCheck.h"
#include <cassert>

void TypeCheck::visit(const PExpr &node) {
  if (node->visitedTc == Expr::VisitState::PermMark)
    return;
  else if (node->visitedTc == Expr::VisitState::TempMark) {
    throw std::runtime_error("Cycle detected");
  }
  node->visitedTc = Expr::VisitState::TempMark;
  if (auto binOp = std::dynamic_pointer_cast<BinOp>(node)) {
    visit(binOp->op1);
    visit(binOp->op2);
    node->visitedTc = Expr::VisitState::PermMark;
  } else if (auto neg = std::dynamic_pointer_cast<Negate>(node)) {
    visit(neg->op);
    node->visitedTc = Expr::VisitState::PermMark;
  } else if (auto val = std::dynamic_pointer_cast<Value>(node)) {
    node->visitedTc = Expr::VisitState::PermMark;
  } else if (auto name = std::dynamic_pointer_cast<Constant>(node)) {
    node->visitedTc = Expr::VisitState::PermMark;
  } else if (auto assign = std::dynamic_pointer_cast<Assignment>(node)) {
    visit(assign->value);
    node->visitedTc = Expr::VisitState::PermMark;
  } else if (auto func = std::dynamic_pointer_cast<FunctionCall>(node)) {
    visit(func->args);
    auto name = func->name;
    node->visitedTc = Expr::VisitState::PermMark;
    if (name == "sin" || name == "cos") {
      assert(func->args->args.size() == 1);
    } else {
      throw std::runtime_error("Unknown function " + name);
    }
  } else if (auto args = std::dynamic_pointer_cast<Arguments>(node)) {
  } else {
    throw std::runtime_error(std::string("Unknown node type ") +
                             typeid(node).name());
  }
}

void TypeCheck::typecheck(const PExpr &node) { visit(node); }
