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
  }
}

Code::Code(COpType t, Ref *op1, Ref *op2) : type(t), op1(op1), op2(op2) {}
void Code::print(const std::list<Ref *> &code) {
  std::cout << addr(code) << ": " << opToStr(type) << " " << op1->addr(code)
            << ", " << op2->addr(code) << std::endl;
}
std::string Code::addr(const std::list<Ref *> &code) {
  std::stringstream ss;
  ss << "$"
     << std::distance(code.cbegin(),
                      std::find(code.cbegin(), code.cend(), this));
  return ss.str();
}

NameRef::NameRef(std::string s) : nameRef(s) {}
void NameRef::print(const std::list<Ref *> &) {}
std::string NameRef::addr(const std::list<Ref *> &) { return nameRef; }

ValRef::ValRef(double val) : valRef(val) {}
void ValRef::print(const std::list<Ref *> &) {}
std::string ValRef::addr(const std::list<Ref *> &) {
  return std::to_string(valRef);
}
