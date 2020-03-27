#include "parser.h"
#include <iostream>

Parser::Parser(Lexer *lex) : lex(lex) { nextToken(); }

void Parser::nextToken() { curTok = lex->getNextToken(); }

std::shared_ptr<Expr> Parser::parse() { return E(); }

std::shared_ptr<Expr> Parser::makeExpr(OpType t, std::shared_ptr<Expr> e1,
                                       std::shared_ptr<Expr> e2) {
  BinOp op(t, e1, e2);
  for (auto &i : knownExprs) {
    if (*i == op)
      return i;
  }
  std::shared_ptr<Expr> newExpr = std::make_shared<BinOp>(t, e1, e2);
  knownExprs.push_back(newExpr);
  return newExpr;
}
std::shared_ptr<Expr> Parser::makeExpr(std::shared_ptr<Expr> e) {
  Negate op(e);
  for (auto &i : knownExprs) {
    if (*i == op)
      return i;
  }
  std::shared_ptr<Expr> newExpr = std::make_shared<Negate>(e);
  knownExprs.push_back(newExpr);
  return newExpr;
}
std::shared_ptr<Expr> Parser::makeExpr(double val) {
  Value op(val);
  for (auto &i : knownExprs) {
    if (*i == op)
      return i;
  }
  std::shared_ptr<Expr> newExpr = std::make_shared<Value>(val);
  knownExprs.push_back(newExpr);
  return newExpr;
}
std::shared_ptr<Expr> Parser::makeExpr(std::string name) {
  Constant op(name);
  for (auto &i : knownExprs) {
    if (*i == op)
      return i;
  }
  std::shared_ptr<Expr> newExpr = std::make_shared<Constant>(name);
  knownExprs.push_back(newExpr);
  return newExpr;
}

std::shared_ptr<Expr> Parser::E() {
  // std::cout << "E -> T E'" << std::endl;
  std::shared_ptr<Expr> t = T();
  return E1(t);
}
std::shared_ptr<Expr> Parser::E1(std::shared_ptr<Expr> t1) {
  if (curTok.type == TokenType::Operator && curTok.attribute == "+") {
    // std::cout << "E' -> + T E'" << std::endl;
    nextToken();
    std::shared_ptr<Expr> t2 = T();
    return E1(makeExpr(OpType::Add, t1, t2));
  } else if (curTok.type == TokenType::Operator && curTok.attribute == "-") {
    // std::cout << "E' -> - T E'" << std::endl;
    nextToken();
    std::shared_ptr<Expr> t2 = T();
    return E1(makeExpr(OpType::Sub, t1, t2));
  } else {
    // std::cout << "E' -> ε" << std::endl;
    return t1;
  }
}
std::shared_ptr<Expr> Parser::T() {
  // std::cout << "T -> F T'" << std::endl;
  std::shared_ptr<Expr> f = F();
  return T1(f);
}
std::shared_ptr<Expr> Parser::T1(std::shared_ptr<Expr> f1) {
  if (curTok.type == TokenType::Operator && curTok.attribute == "*") {
    // std::cout << "T' -> * F T'" << std::endl;
    nextToken();
    std::shared_ptr<Expr> f2 = F();
    return T1(makeExpr(OpType::Mul, f1, f2));
  } else if (curTok.type == TokenType::Operator && curTok.attribute == "/") {
    // std::cout << "T' -> / F T'" << std::endl;
    nextToken();
    std::shared_ptr<Expr> f2 = F();
    return T1(makeExpr(OpType::Div, f1, f2));
  } else {
    // std::cout << "T' -> ε" << std::endl;
    return f1;
  }
}
std::shared_ptr<Expr> Parser::F() {
  // std::cout << "F -> V F'" << std::endl;
  std::shared_ptr<Expr> v = V();
  return F1(v);
}
std::shared_ptr<Expr> Parser::F1(std::shared_ptr<Expr> v) {
  if (curTok.type == TokenType::Operator && curTok.attribute == "^") {
    // std::cout << "F' -> ^ F" << std::endl;
    nextToken();
    std::shared_ptr<Expr> f = F();
    return makeExpr(OpType::Exp, v, f);
  } else {
    // std::cout << "F' -> ε" << std::endl;
    return v;
  }
}
std::shared_ptr<Expr> Parser::V() {
  if (curTok.type == TokenType::LParen) {
    // std::cout << "V -> ( E )" << std::endl;
    nextToken();
    std::shared_ptr<Expr> result = E();
    if (curTok.type != TokenType::RParen)
      throw new std::runtime_error("Expected ), got " + curTok.attribute);
    nextToken();
    return result;
  } else if (curTok.type == TokenType::Id) {
    // std::cout << "V -> id" << std::endl;
    std::shared_ptr<Expr> result = makeExpr(curTok.attribute);
    nextToken();
    return result;
  } else if (curTok.type == TokenType::Number) {
    // std::cout << "V -> number" << std::endl;
    std::shared_ptr<Expr> result = makeExpr(std::stod(curTok.attribute));
    nextToken();
    return result;
  } else if (curTok.type == TokenType::Operator && curTok.attribute == "-") {
    // std::cout << "V -> - V" << std::endl;
    nextToken();
    return makeExpr(V());
  } else {
    throw new std::runtime_error("Expected (, id, number or -, got " +
                                 curTok.attribute);
  }
}

std::string opTypeToStr(OpType type) {
  switch (type) {
  case OpType::Add:
    return "+";
  case OpType::Sub:
    return "-";
  case OpType::Mul:
    return "*";
  case OpType::Div:
    return "/";
  case OpType::Exp:
    return "^";
  }
}
