#include "code.h"
#include "interpret.h"
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
    GraphToNode g2n;
    auto code = g2n.graphToCode(expr);
    for (auto &i : code) {
      i->print(code);
    }
    Interpreter intr;
    auto res = intr.interpret(constants, code);
    std::cout << res << std::endl;
  }
}
