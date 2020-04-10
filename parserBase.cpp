#include "parserBase.h"
#include <cmath>
#include <iostream>

BinOp::BinOp(OpType type, std::shared_ptr<Expr> op1, std::shared_ptr<Expr> op2)
    : type(type), op1(op1), op2(op2) {}
void BinOp::print(std::string indent) {
  std::cout << indent << opTypeToStr(type) << "(" << std::endl;
  op1->print(indent + "  ");
  op2->print(indent + "  ");
  std::cout << indent << ")" << std::endl;
}
double BinOp::compute(const std::map<std::string, double> &ctx) {
  switch (type) {
  case OpType::Add:
    return op1->compute(ctx) + op2->compute(ctx);
  case OpType::Sub:
    return op1->compute(ctx) - op2->compute(ctx);
  case OpType::Mul:
    return op1->compute(ctx) * op2->compute(ctx);
  case OpType::Div:
    return op1->compute(ctx) / op2->compute(ctx);
  case OpType::Exp:
    return std::pow(op1->compute(ctx), op2->compute(ctx));
  }
}
bool BinOp::operator==(const Expr &other) {
  auto o = dynamic_cast<const BinOp *>(&other);
  if (!o)
    return false;
  return type == o->type && op1 == o->op1 && op2 == o->op2;
}

////////////////////////////////////////////////////////////////

Negate::Negate(std::shared_ptr<Expr> op) : op(op) {}
void Negate::print(std::string indent) {
  std::cout << indent << "neg(" << std::endl;
  op->print(indent + "  ");
  std::cout << indent << ")" << std::endl;
}
double Negate::compute(const std::map<std::string, double> &ctx) {
  return -op->compute(ctx);
}
bool Negate::operator==(const Expr &other) {
  auto o = dynamic_cast<const Negate *>(&other);
  if (!o)
    return false;
  return op == o->op;
}

///////////////////////////////////////////////////////////////

Value::Value(double val) : val(val) {}
void Value::print(std::string indent) {
  std::cout << indent << val << std::endl;
}
double Value::compute(const std::map<std::string, double> &) { return val; }
bool Value::operator==(const Expr &other) {
  auto o = dynamic_cast<const Value *>(&other);
  if (!o)
    return false;
  return val == o->val;
}

///////////////////////////////////////////////////////////////

Constant::Constant(std::string name) : name(name) {}
void Constant::print(std::string indent) {
  std::cout << indent << name << std::endl;
}
double Constant::compute(const std::map<std::string, double> &ctx) {
  return ctx.at(name);
}
bool Constant::operator==(const Expr &other) {
  auto o = dynamic_cast<const Constant *>(&other);
  if (!o)
    return false;
  return name == o->name;
}

////////////////////////// ParserBase /////////////////////////

ParserBase::ParserBase() {}
ParserBase::~ParserBase() {}

std::shared_ptr<Expr> ParserBase::makeExpr(OpType t, std::shared_ptr<Expr> e1,
                                           std::shared_ptr<Expr> e2) {
  BinOp op(t, e1, e2);
  for (auto &i : knownExprs) {
    if (*i == op)
      return i;
  }
  std::shared_ptr<Expr> newExpr = std::make_shared<BinOp>(t, e1, e2);
  knownExprs.push_back(newExpr);
  return newExpr;
}
std::shared_ptr<Expr> ParserBase::makeExpr(std::shared_ptr<Expr> e) {
  Negate op(e);
  for (auto &i : knownExprs) {
    if (*i == op)
      return i;
  }
  std::shared_ptr<Expr> newExpr = std::make_shared<Negate>(e);
  knownExprs.push_back(newExpr);
  return newExpr;
}
std::shared_ptr<Expr> ParserBase::makeExpr(double val) {
  Value op(val);
  for (auto &i : knownExprs) {
    if (*i == op)
      return i;
  }
  std::shared_ptr<Expr> newExpr = std::make_shared<Value>(val);
  knownExprs.push_back(newExpr);
  return newExpr;
}
std::shared_ptr<Expr> ParserBase::makeExpr(std::string name) {
  Constant op(name);
  for (auto &i : knownExprs) {
    if (*i == op)
      return i;
  }
  std::shared_ptr<Expr> newExpr = std::make_shared<Constant>(name);
  knownExprs.push_back(newExpr);
  return newExpr;
}

std::string opTypeToStr(OpType type) {
  static const char *n[] = {"+", "-", "*", "/", "^"};
  return n[static_cast<std::size_t>(type)];
}
