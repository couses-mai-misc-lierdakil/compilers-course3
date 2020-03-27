#include "code.h"
#include <iostream>

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

CodeParser::CodeParser(Lexer *lex) : lex(lex) { nextToken(); }

void CodeParser::nextToken() { curTok = lex->getNextToken(); }

const std::list<Ref *> &CodeParser::parse() {
  E();
  return code;
}

Ref *CodeParser::E() {
  // std::cout << "E -> T E'" << std::endl;
  Ref *t = T();
  return E1(t);
}
Ref *CodeParser::E1(Ref *t1) {
  if (curTok.type == TokenType::Operator && curTok.attribute == "+") {
    // std::cout << "E' -> + T E'" << std::endl;
    nextToken();
    Ref *t2 = T();
    Ref *result = new Code(COpType::Add, t1, t2);
    code.push_back(result);
    return E1(result);
  } else if (curTok.type == TokenType::Operator && curTok.attribute == "-") {
    // std::cout << "E' -> - T E'" << std::endl;
    nextToken();
    Ref *t2 = T();
    Ref *result = new Code(COpType::Sub, t1, t2);
    code.push_back(result);
    return E1(result);
  } else {
    // std::cout << "E' -> ε" << std::endl;
    return t1;
  }
}
Ref *CodeParser::T() {
  // std::cout << "T -> F T'" << std::endl;
  Ref *f = F();
  return T1(f);
}
Ref *CodeParser::T1(Ref *f1) {
  if (curTok.type == TokenType::Operator && curTok.attribute == "*") {
    // std::cout << "T' -> * F T'" << std::endl;
    nextToken();
    Ref *f2 = F();
    Ref *result = new Code(COpType::Mul, f1, f2);
    code.push_back(result);
    return T1(result);
  } else if (curTok.type == TokenType::Operator && curTok.attribute == "/") {
    // std::cout << "T' -> / F T'" << std::endl;
    nextToken();
    Ref *f2 = F();
    Ref *result = new Code(COpType::Div, f1, f2);
    code.push_back(result);
    return T1(result);
  } else {
    // std::cout << "T' -> ε" << std::endl;
    return f1;
  }
}
Ref *CodeParser::F() {
  // std::cout << "F -> V F'" << std::endl;
  Ref *v = V();
  return F1(v);
}
Ref *CodeParser::F1(Ref *v) {
  if (curTok.type == TokenType::Operator && curTok.attribute == "^") {
    // std::cout << "F' -> ^ F" << std::endl;
    nextToken();
    Ref *f = F();
    Ref *result = new Code(COpType::Exp, v, f);
    code.push_back(result);
    return result;
  } else {
    // std::cout << "F' -> ε" << std::endl;
    return v;
  }
}
Ref *CodeParser::V() {
  if (curTok.type == TokenType::LParen) {
    // std::cout << "V -> ( E )" << std::endl;
    nextToken();
    Ref *result = E();
    if (curTok.type != TokenType::RParen)
      throw new std::runtime_error("Expected ), got " + curTok.attribute);
    nextToken();
    return result;
  } else if (curTok.type == TokenType::Id) {
    // std::cout << "V -> id" << std::endl;
    Ref *result = new NameRef{curTok.attribute};
    code.push_back(result);
    nextToken();
    return result;
  } else if (curTok.type == TokenType::Number) {
    // std::cout << "V -> number" << std::endl;
    Ref *result = new ValRef(std::stod(curTok.attribute));
    code.push_back(result);
    nextToken();
    return result;
  } else if (curTok.type == TokenType::Operator && curTok.attribute == "-") {
    // std::cout << "V -> - V" << std::endl;
    nextToken();
    auto argRef = V();
    Ref *result = new Code(COpType::Neg, argRef, nullptr);
    code.push_back(result);
    return result;
  } else {
    throw new std::runtime_error("Expected (, id, number or -, got " +
                                 curTok.attribute);
  }
}
