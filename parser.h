#ifndef PARSER_H
#define PARSER_H
#include "lexer.h"
#include <cmath>
#include <iostream>
#include <list>
#include <map>
#include <memory>

class Expr {
public:
  virtual ~Expr() {}
  virtual void print(std::string indent) = 0;
  virtual double compute(const std::map<std::string, double> &context) = 0;
  virtual bool operator==(const Expr &other) = 0;
};

enum class OpType { Add, Sub, Mul, Div, Exp };

std::string opTypeToStr(OpType type);

class BinOp : public Expr {
  OpType type;
  std::shared_ptr<Expr> op1;
  std::shared_ptr<Expr> op2;

public:
  BinOp(OpType type, std::shared_ptr<Expr> op1, std::shared_ptr<Expr> op2)
      : type(type), op1(op1), op2(op2) {}
  ~BinOp() {}
  void print(std::string indent) {
    std::cout << indent << opTypeToStr(type) << "(" << std::endl;
    op1->print(indent + "  ");
    op2->print(indent + "  ");
    std::cout << indent << ")" << std::endl;
  }
  double compute(const std::map<std::string, double> &ctx) {
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
  bool operator==(const Expr &other) {
    auto o = dynamic_cast<const BinOp *>(&other);
    if (!o)
      return false;
    return type == o->type && op1 == o->op1 && op2 == o->op2;
  }
};

class Negate : public Expr {
  std::shared_ptr<Expr> op;

public:
  Negate(std::shared_ptr<Expr> op) : op(op) {}
  ~Negate() {}
  void print(std::string indent) {
    std::cout << indent << "neg(" << std::endl;
    op->print(indent + "  ");
    std::cout << indent << ")" << std::endl;
  }
  double compute(const std::map<std::string, double> &ctx) {
    return -op->compute(ctx);
  }
  bool operator==(const Expr &other) {
    auto o = dynamic_cast<const Negate *>(&other);
    if (!o)
      return false;
    return op == o->op;
  }
};

class Value : public Expr {
  double val;

public:
  Value(double val) : val(val) {}
  void print(std::string indent) { std::cout << indent << val << std::endl; }
  double compute(const std::map<std::string, double> &) { return val; }
  bool operator==(const Expr &other) {
    auto o = dynamic_cast<const Value *>(&other);
    if (!o)
      return false;
    return val == o->val;
  }
};

class Constant : public Expr {
  std::string name;

public:
  Constant(std::string name) : name(name) {}
  void print(std::string indent) { std::cout << indent << name << std::endl; }
  double compute(const std::map<std::string, double> &ctx) {
    return ctx.at(name);
  }
  bool operator==(const Expr &other) {
    auto o = dynamic_cast<const Constant *>(&other);
    if (!o)
      return false;
    return name == o->name;
  }
};

class Parser {
  Lexer *lex;
  Token curTok;
  std::list<std::shared_ptr<Expr>> knownExprs;
  std::shared_ptr<Expr> makeExpr(OpType, std::shared_ptr<Expr>,
                                 std::shared_ptr<Expr>);
  std::shared_ptr<Expr> makeExpr(std::shared_ptr<Expr>);
  std::shared_ptr<Expr> makeExpr(double);
  std::shared_ptr<Expr> makeExpr(std::string);
  void nextToken();
  std::shared_ptr<Expr> E();
  std::shared_ptr<Expr> E1(std::shared_ptr<Expr> t);
  std::shared_ptr<Expr> T();
  std::shared_ptr<Expr> T1(std::shared_ptr<Expr> f);
  std::shared_ptr<Expr> F();
  std::shared_ptr<Expr> F1(std::shared_ptr<Expr> v);
  std::shared_ptr<Expr> V();

public:
  Parser(Lexer *lex);
  std::shared_ptr<Expr> parse();
};
#endif
