#include "code.h"
#include <algorithm>
#include <iostream>
#include <sstream>

std::string opToStr(COpType t) {
  switch (t) {
  case COpType::Assign:
    return "assign";
  case COpType::Add:
    return "add";
  case COpType::Sub:
    return "sub";
  case COpType::Mul:
    return "mul";
  case COpType::Div:
    return "div";
  case COpType::Exp:
    return "exp";
  case COpType::Neg:
    return "neg";
  case COpType::Call:
    return "call";
  case COpType::Param:
    return "param";
  case COpType::GetParam:
    return "getParam";
  }
}

Code::Code(COpType t, PRef op1, PRef op2) : type(t), op1(op1), op2(op2) {}
void Code::print(const std::list<PRef> &code) {
  if (op1 && op2) {
    std::cout << addr(code) << ": " << opToStr(type) << " " << op1->addr(code)
              << ", " << op2->addr(code) << std::endl;
  } else if (op1) {
    std::cout << addr(code) << ": " << opToStr(type) << " " << op1->addr(code)
              << std::endl;
  } else {
    std::cout << addr(code) << ": " << opToStr(type) << std::endl;
  }
}
std::string Code::addr(const std::list<std::shared_ptr<Ref>> &code) {
  std::stringstream ss;
  ss << "$"
     << std::distance(code.cbegin(), std::find(code.cbegin(), code.cend(),
                                               shared_from_this()));
  return ss.str();
}

NameRef::NameRef(std::string s) : nameRef(s) {}
void NameRef::print(const std::list<PRef> &) {}
std::string NameRef::addr(const std::list<PRef> &) { return nameRef; }

ValRef::ValRef(double val) : valRef(val) {}
void ValRef::print(const std::list<PRef> &) {}
std::string ValRef::addr(const std::list<PRef> &) {
  return std::to_string(valRef);
}

///////////////////////////////////////////////////

std::shared_ptr<Ref> GraphToNode::visit(std::shared_ptr<Expr> node) {
  if (node->visited == Expr::VisitState::PermMark)
    return resultMap[node];
  else if (node->visited == Expr::VisitState::TempMark) {
    throw std::runtime_error("Cycle detected");
  }
  node->visited = Expr::VisitState::TempMark;
  if (auto binOp = std::dynamic_pointer_cast<BinOp>(node)) {
    auto opRef1 = visit(binOp->op1);
    auto opRef2 = visit(binOp->op2);
    node->visited = Expr::VisitState::PermMark;
    COpType opType;
    switch (binOp->type) {
    case OpType::Add:
      opType = COpType::Add;
      break;
    case OpType::Div:
      opType = COpType::Div;
      break;
    case OpType::Exp:
      opType = COpType::Exp;
      break;
    case OpType::Sub:
      opType = COpType::Sub;
      break;
    case OpType::Mul:
      opType = COpType::Mul;
      break;
    }
    auto res = std::make_shared<Code>(opType, opRef1, opRef2);
    resultMap[node] = res;
    sorted.push_back(res);
    return res;
  } else if (auto neg = std::dynamic_pointer_cast<Negate>(node)) {
    auto opRef = visit(neg->op);
    node->visited = Expr::VisitState::PermMark;
    auto res = std::make_shared<Code>(COpType::Neg, opRef, nullptr);
    resultMap[node] = res;
    sorted.push_back(res);
    return res;
  } else if (auto val = std::dynamic_pointer_cast<Value>(node)) {
    node->visited = Expr::VisitState::PermMark;
    auto res = std::make_shared<ValRef>(val->val);
    resultMap[node] = res;
    sorted.push_back(res);
    return res;
  } else if (auto name = std::dynamic_pointer_cast<Constant>(node)) {
    node->visited = Expr::VisitState::PermMark;
    auto res = std::make_shared<NameRef>(name->name);
    resultMap[node] = res;
    sorted.push_back(res);
    return res;
  } else if (auto assign = std::dynamic_pointer_cast<Assignment>(node)) {
    auto val = visit(assign->value);
    node->visited = Expr::VisitState::PermMark;
    auto name = std::make_shared<NameRef>(assign->varName);
    auto res = std::make_shared<Code>(COpType::Assign, name, val);
    resultMap[node] = res;
    sorted.push_back(res);
    return res;
  } else if (auto func = std::dynamic_pointer_cast<FunctionCall>(node)) {
    visit(func->args);
    node->visited = Expr::VisitState::PermMark;
    auto name = func->name;
    auto res = std::make_shared<Code>(COpType::Call,
                                      std::make_shared<NameRef>(name), nullptr);
    resultMap[node] = res;
    sorted.push_back(res);
    return res;
  } else if (auto args = std::dynamic_pointer_cast<Arguments>(node)) {
    std::list<PRef> argCodes;
    for (auto &arg : args->args) {
      auto argCode = visit(arg);
      argCodes.push_back(
          std::make_shared<Code>(COpType::Param, argCode, nullptr));
    }
    node->visited = Expr::VisitState::PermMark;
    sorted.splice(sorted.end(), argCodes);
    return nullptr;
  } else if (auto fdef = std::dynamic_pointer_cast<FunctionDef>(node)) {
    visit(fdef->def);
    visit(fdef->expr);
    node->visited = Expr::VisitState::PermMark;
    return nullptr;
  } else if (auto args = std::dynamic_pointer_cast<DefArgs>(node)) {
    for (auto arg = args->args.rbegin(); arg != args->args.rend(); ++arg) {
      auto name = std::make_shared<NameRef>(*arg);
      auto cmd = std::make_shared<Code>(COpType::GetParam, nullptr, nullptr);
      sorted.push_back(cmd);
      auto cmd2 = std::make_shared<Code>(COpType::Assign, name, cmd);
      sorted.push_back(cmd2);
    }
    node->visited = Expr::VisitState::PermMark;
    return nullptr;
  } else {
    throw std::runtime_error(std::string("Unknown node type ") +
                             typeid(node).name());
  }
}

GraphToNode::ResultT GraphToNode::graphToCode(std::shared_ptr<Expr> root) {
  sorted.clear();
  resultMap.clear();
  visit(root);
  return std::move(sorted);
}