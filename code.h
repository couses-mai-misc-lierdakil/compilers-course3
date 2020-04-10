#ifndef CODE_H
#define CODE_H
#include <list>
#include <string>
#include <memory>
#include "parserBase.h"

enum class COpType { Assign, Add, Sub, Mul, Div, Exp, Neg, Call, Param };

std::string opToStr(COpType);

struct Ref;

using PRef = std::shared_ptr<Ref>;

struct Ref : public std::enable_shared_from_this<Ref> {
  virtual ~Ref(){};
  virtual void print(const std::list<PRef> &code) = 0;
  virtual std::string addr(const std::list<PRef> &code) = 0;
};

struct Code : public Ref {
  COpType type;
  PRef op1;
  PRef op2;
  Code(COpType t, PRef op1, PRef op2);
  void print(const std::list<PRef> &code);
  std::string addr(const std::list<PRef> &code);
};

struct NameRef : public Ref {
  std::string nameRef;
  NameRef(std::string s);
  void print(const std::list<PRef > &);
  std::string addr(const std::list<PRef > &);
};

struct ValRef : public Ref {
  double valRef;
  ValRef(double val);
  void print(const std::list<PRef > &);
  std::string addr(const std::list<PRef > &);
};

class GraphToNode {
  using ResultT = std::list<PRef>;
  ResultT sorted;
  PRef visit(std::shared_ptr<Expr> node);
  std::map<std::shared_ptr<Expr>, PRef> resultMap;
public:
  ResultT graphToCode(std::shared_ptr<Expr> root);
};

#endif
