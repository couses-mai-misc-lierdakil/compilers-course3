#include "code.h"
#include "parser.h"
#include <iostream>

std::string tokTypeToStr(TokenType t) {
  switch (t) {
  case TokenType::Number:
    return "Number";
  case TokenType::Operator:
    return "Operator";
  case TokenType::Id:
    return "Id";
  case TokenType::LParen:
    return "LParen";
  case TokenType::RParen:
    return "RParen";
  case TokenType::Comma:
    return "Comma";
  case TokenType::Invalid:
    return "Invalid";
  }
}

int main() {
  std::string line;
  std::map<std::string, double> constants;
  constants["pi"] = M_PI;
  while (std::getline(std::cin, line)) {
    Lexer lex(line);
    Parser parser(&lex);
    auto expr = parser.parse();
    expr->print("");
    std::cout << expr->compute(constants) << std::endl;

    Lexer lex2(line);
    CodeParser cparser(&lex2);
    auto code = cparser.parse();
    for (auto &i : code) {
      i->print(code);
    }
    // while (true) {
    //   auto t = lex.getNextToken();
    //   std::cout << tokTypeToStr(t.type) << "(" << t.attribute << ")"
    //             << std::endl;
    //   if (t.type == TokenType::Invalid)
    //     break;
    // }
  }
}
