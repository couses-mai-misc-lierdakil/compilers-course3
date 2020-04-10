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
