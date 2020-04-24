#include "code.h"
#include "interpret.h"
#include "parser.h"
#include "typeCheck.h"
#include <cmath>
#include <iostream>

struct print {
  void operator()(int val) { std::cout << "i" << val << std::endl; }
  void operator()(double val) { std::cout << val << std::endl; }
};

int main() {
  std::string line;
  Interpreter::VarMap variables;
  Interpreter::FuncMap functions;
  TypeCheck::VarTypes varTypes;
  TypeCheck::FuncRetTypes funcRetTypes;
  variables["pi"] = M_PI;
  varTypes["pi"] = Type::FloatType;
  while (std::getline(std::cin, line)) {
    try {
      Lexer lex(line);
      Parser parser(&lex);
      auto expr = parser.parse();
      expr->print("");
      TypeCheck tc(functions, varTypes, funcRetTypes);
      tc.typecheck(expr);
      std::cerr << "Finished typecheck" << std::endl;
      GraphToNode g2n;
      auto code = g2n.graphToCode(expr);
      std::cerr << "Finished codegen" << std::endl;
      for (auto &i : code) {
        i->print(code);
      }
      if (auto fun = std::dynamic_pointer_cast<FunctionDef>(expr)) {
        functions[{fun->name, fun->arity()}] = code;
      } else {
        Interpreter intr;
        auto res = intr.interpret(variables, functions, code);
        std::visit(print(), res);
      }
    } catch (std::exception &e) {
      std::cerr << e.what() << std::endl;
    }
  }
}
