#ifndef CODE_H
#define CODE_H
#include "lexer.h"
#include <algorithm>
#include <iostream>
#include <list>
#include <sstream>
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
  Code(COpType t, Ref *op1, Ref *op2) : type(t), op1(op1), op2(op2) {}
  void print(const std::list<Ref *> &code) {
    std::cout << addr(code) << ": " << opToStr(type) << " " << op1->addr(code)
              << ", " << op2->addr(code) << std::endl;
  }
  std::string addr(const std::list<Ref *> &code) {
    std::stringstream ss;
    ss << "$"
       << std::distance(code.cbegin(),
                        std::find(code.cbegin(), code.cend(), this));
    return ss.str();
  }
};

struct NameRef : public Ref {
  std::string nameRef;
  NameRef(std::string s) : nameRef(s) {}
  void print(const std::list<Ref *> &) {}
  std::string addr(const std::list<Ref *> &) { return nameRef; }
};

struct ValRef : public Ref {
  double valRef;
  ValRef(double val) : valRef(val) {}
  void print(const std::list<Ref *> &) {}
  std::string addr(const std::list<Ref *> &) {
    std::stringstream ss;
    ss << valRef;
    return ss.str();
  }
};

class CodeParser {
  Lexer *lex;
  Token curTok;
  std::list<Ref *> code;
  void nextToken();
  Ref *E();
  Ref *E1(Ref *t);
  Ref *T();
  Ref *T1(Ref *f);
  Ref *F();
  Ref *F1(Ref *v);
  Ref *V();

public:
  CodeParser(Lexer *lex);
  const std::list<Ref *> &parse();
};
#endif
