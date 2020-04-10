#ifndef PARSER_BASE_H
#define PARSER_BASE_H

#include <list>
#include <map>
#include <memory>

class Expr;

using PExpr = std::shared_ptr<Expr>;

class Expr {
public:
  virtual ~Expr() {}
  virtual void print(std::string indent) = 0;
  virtual double compute(std::map<std::string, double> &context) = 0;
  virtual bool operator==(const Expr &other) = 0;
  enum class VisitState { NotVisited, TempMark, PermMark };
  VisitState visited = VisitState::NotVisited;
  VisitState visitedTc = VisitState::NotVisited;
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
  std::list<std::string> args;
  DefArgs() {}
  DefArgs(std::string arg) : args({arg}) {}
  DefArgs(PExpr def, std::string arg)
      : args(std::move(std::static_pointer_cast<DefArgs>(def)->args)) {
    args.push_back(arg);
  }
  void print(std::string indent) {}
  double compute(std::map<std::string, double> &ctx) { return 0; }
  bool operator==(const Expr &other) { return false; }
};

class FunctionDef : public Expr {
public:
  std::string name;
  std::shared_ptr<DefArgs> def;
  PExpr expr;
  FunctionDef(std::string name, PExpr def, PExpr expr)
      : name(name), def(std::static_pointer_cast<DefArgs>(def)), expr(expr) {}
};

class FunctionCall : public Expr {
public:
  std::string name;
  std::shared_ptr<Arguments> args;
  FunctionCall(std::string, PExpr);
  void print(std::string indent);
  double compute(std::map<std::string, double> &ctx);
  bool operator==(const Expr &other);
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

public:
  ParserBase();
  virtual ~ParserBase();
};

using ResultType = PExpr;
#endif
