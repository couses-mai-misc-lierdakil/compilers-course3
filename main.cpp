#include "code.h"
#include "parser.h"
#include <cmath>
#include <iostream>

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
  }
}
