#ifndef CODE_H
#define CODE_H
#include <list>
#include <string>

enum class COpType { Assign, Add, Sub, Mul, Div, Exp, Neg };

std::string opToStr(COpType);

struct Ref {
  virtual ~Ref(){};
  virtual void print(const std::list<Ref *> &code) = 0;
  virtual std::string addr(const std::list<Ref *> &code) = 0;
};

struct Code : public Ref {
  COpType type;
  Ref *op1;
  Ref *op2;
  Code(COpType t, Ref *op1, Ref *op2);
  void print(const std::list<Ref *> &code);
  std::string addr(const std::list<Ref *> &code);
};

struct NameRef : public Ref {
  std::string nameRef;
  NameRef(std::string s);
  void print(const std::list<Ref *> &);
  std::string addr(const std::list<Ref *> &);
};

struct ValRef : public Ref {
  double valRef;
  ValRef(double val);
  void print(const std::list<Ref *> &);
  std::string addr(const std::list<Ref *> &);
};

#endif
