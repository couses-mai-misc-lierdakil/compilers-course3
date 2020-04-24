#ifndef PARSER_BASE_H
#define PARSER_BASE_H

#include <algorithm>
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <vector>

class Expr;

using PExpr = std::shared_ptr<Expr>;

enum class Type { NoType, IntType, FloatType };

class Expr {
public:
  virtual ~Expr() {}
  virtual void print(std::string indent) = 0;
  virtual double compute(std::map<std::string, double> &context) = 0;
  virtual bool operator==(const Expr &other) = 0;
  enum class VisitState { NotVisited, TempMark, PermMark };
  VisitState visited = VisitState::NotVisited;
  VisitState visitedTc = VisitState::NotVisited;
  Type type = Type::NoType;
};

struct TypeNode : public Expr {
  Type t;
  TypeNode(Type t) : t(t) {}
  void print(std::string indent) {
    std::cout << indent << std::size_t(t) << std::endl;
  }
  double compute(std::map<std::string, double> &context) { return 0; }
  bool operator==(const Expr &other) {
    auto o = dynamic_cast<const TypeNode *>(&other);
    if (!o)
      return false;
    return o->t == t;
  }
};

enum class OpType : std::size_t { Add, Sub, Mul, Div, Exp };

std::string opTypeToStr(OpType type);

class BinOp : public Expr {
public:
  OpType type;
  PExpr op1;
  PExpr op2;
  BinOp(OpType type, PExpr op1, PExpr op2);
  void print(std::string indent);
  double compute(std::map<std::string, double> &ctx);
  bool operator==(const Expr &other);
};

class Negate : public Expr {
public:
  PExpr op;
  Negate(PExpr op);
  void print(std::string indent);
  double compute(std::map<std::string, double> &ctx);
  bool operator==(const Expr &other);
};

class Value : public Expr {
public:
  double val;
  Value(double val);
  void print(std::string indent);
  double compute(std::map<std::string, double> &);
  bool operator==(const Expr &other);
};

class IntValue : public Expr {
public:
  int val;
  IntValue(int val);
  void print(std::string indent);
  double compute(std::map<std::string, double> &);
  bool operator==(const Expr &other);
};

class Constant : public Expr {
public:
  std::string name;
  Constant(std::string name);
  void print(std::string indent);
  double compute(std::map<std::string, double> &ctx);
  bool operator==(const Expr &other);
};

class Arguments : public Expr {
public:
  std::list<PExpr> args;
  Arguments();
  Arguments(PExpr);
  Arguments(PExpr, PExpr);
  void print(std::string indent);
  double compute(std::map<std::string, double> &ctx);
  bool operator==(const Expr &other);
};

class DefArgs : public Expr {
public:
  std::list<std::pair<std::string, Type>> args;
  DefArgs() {}
  DefArgs(PExpr t, std::string arg)
      : args({{arg, std::static_pointer_cast<TypeNode>(t)->t}}) {}
  DefArgs(PExpr def, PExpr t, std::string arg)
      : args(std::move(std::static_pointer_cast<DefArgs>(def)->args)) {
    args.push_back({arg, std::static_pointer_cast<TypeNode>(t)->t});
  }
  void print(std::string indent) {}
  double compute(std::map<std::string, double> &ctx) { return 0; }
  bool operator==(const Expr &other) { return false; }
};

class FunctionDef : public Expr {
public:
  std::string name;
  std::shared_ptr<DefArgs> def;
  std::vector<Type> argTypes;
  PExpr expr;
  FunctionDef(std::string name, PExpr def, PExpr expr)
      : name(name), def(std::static_pointer_cast<DefArgs>(def)), expr(expr) {
    this->argTypes.reserve(this->def->args.size());
    for (auto &i : this->def->args) {
      this->argTypes.push_back(i.second);
    }
  }
  void print(std::string indent) {}
  double compute(std::map<std::string, double> &ctx) { return 0; }
  bool operator==(const Expr &other) { return false; }
  std::size_t arity() { return def->args.size(); }
};

class FunctionCall : public Expr {
public:
  std::string name;
  std::shared_ptr<Arguments> args;
  std::vector<Type> argTypes;
  FunctionCall(std::string, PExpr);
  void print(std::string indent);
  double compute(std::map<std::string, double> &ctx);
  bool operator==(const Expr &other);
  std::size_t arity() { return args->args.size(); }
};

class Assignment : public Expr {
public:
  std::string varName;
  PExpr value;
  Assignment(std::string varName, PExpr value);
  void print(std::string indent);
  double compute(std::map<std::string, double> &ctx);
  bool operator==(const Expr &other);
};

class ParserBase {
  std::list<PExpr> knownExprs;

protected:
  PExpr makeExpr(OpType, PExpr, PExpr);
  PExpr makeExpr(PExpr);
  PExpr makeExpr(double);
  PExpr makeExpr(std::string);
  PExpr makeExpr(std::string, PExpr);
  PExpr makeFunCall(std::string, PExpr);
  PExpr makeIntExpr(int);

public:
  ParserBase();
  virtual ~ParserBase();
};

using ResultType = PExpr;
#endif
