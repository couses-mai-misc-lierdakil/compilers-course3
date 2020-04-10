#include "code.h"
#include "interpret.h"
#include "parser.h"
#include "typeCheck.h"
#include <cmath>
#include <iostream>

int main() {
  std::string line;
  std::map<std::string, double> constants;
  std::map<std::pair<std::string, std::size_t>, std::list<PRef>> functions;
  constants["pi"] = M_PI;
  while (std::getline(std::cin, line)) {
    Lexer lex(line);
    Parser parser(&lex);
    auto expr = parser.parse();
    expr->print("");
    TypeCheck tc;
    tc.typecheck(expr);
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
