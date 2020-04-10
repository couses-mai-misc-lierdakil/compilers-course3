#include "parserBase.h"
#include <cmath>
#include <iostream>

BinOp::BinOp(OpType type, PExpr op1, PExpr op2)
    : type(type), op1(op1), op2(op2) {}
void BinOp::print(std::string indent) {
  std::cout << indent << opTypeToStr(type) << "(" << std::endl;
  op1->print(indent + "  ");
  op2->print(indent + "  ");
  std::cout << indent << ")" << std::endl;
}
double BinOp::compute(std::map<std::string, double> &ctx) {
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

Negate::Negate(PExpr op) : op(op) {}
void Negate::print(std::string indent) {
  std::cout << indent << "neg(" << std::endl;
  op->print(indent + "  ");
  std::cout << indent << ")" << std::endl;
}
double Negate::compute(std::map<std::string, double> &ctx) {
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
double Value::compute(std::map<std::string, double> &) { return val; }
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
double Constant::compute(std::map<std::string, double> &ctx) {
  return ctx.at(name);
}
bool Constant::operator==(const Expr &other) {
  auto o = dynamic_cast<const Constant *>(&other);
  if (!o)
    return false;
  return name == o->name;
}

////////////////////////////////////////////////////////////////

Assignment::Assignment(std::string varName, PExpr value)
    : varName(varName), value(value) {}
void Assignment::print(std::string indent) {
  std::cout << indent << varName << "=(" << std::endl;
  value->print(indent + "  ");
  std::cout << indent << ")" << std::endl;
}
double Assignment::compute(std::map<std::string, double> &ctx) {
  auto res = value->compute(ctx);
  ctx[varName] = res;
  return res;
}
bool Assignment::operator==(const Expr &other) { return false; }

////////////////////////// ParserBase /////////////////////////

ParserBase::ParserBase() {}
ParserBase::~ParserBase() {}

PExpr ParserBase::makeExpr(OpType t, PExpr e1, PExpr e2) {
  BinOp op(t, e1, e2);
  for (auto &i : knownExprs) {
    if (*i == op)
      return i;
  }
  PExpr newExpr = std::make_shared<BinOp>(t, e1, e2);
  knownExprs.push_back(newExpr);
  return newExpr;
}
PExpr ParserBase::makeExpr(PExpr e) {
  Negate op(e);
  for (auto &i : knownExprs) {
    if (*i == op)
      return i;
  }
  PExpr newExpr = std::make_shared<Negate>(e);
  knownExprs.push_back(newExpr);
  return newExpr;
}
PExpr ParserBase::makeExpr(double val) {
  Value op(val);
  for (auto &i : knownExprs) {
    if (*i == op)
      return i;
  }
  PExpr newExpr = std::make_shared<Value>(val);
  knownExprs.push_back(newExpr);
  return newExpr;
}
PExpr ParserBase::makeExpr(std::string name) {
  Constant op(name);
  for (auto &i : knownExprs) {
    if (*i == op)
      return i;
  }
  PExpr newExpr = std::make_shared<Constant>(name);
  knownExprs.push_back(newExpr);
  return newExpr;
}

PExpr ParserBase::makeExpr(std::string name, PExpr val) {
  PExpr newExpr = std::make_shared<Assignment>(name, val);
  return newExpr;
}

std::string opTypeToStr(OpType type) {
  static const char *n[] = {"+", "-", "*", "/", "^"};
  return n[static_cast<std::size_t>(type)];
}
