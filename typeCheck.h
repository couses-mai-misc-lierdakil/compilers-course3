#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "interpret.h"
#include "parserBase.h"

class TypeCheck {
public:
  using VarTypes = std::map<std::string, Type>;
  using FuncRetTypes = std::map<std::pair<std::string, std::size_t>,
                                std::pair<Type, std::vector<Type>>>;

private:
  const Interpreter::FuncMap &functions;
  VarTypes &variableTypes;
  FuncRetTypes &functionReturnTypes;
  void visit(VarTypes &localVariableTypes, const PExpr &root);

public:
  TypeCheck(const Interpreter::FuncMap &functions, VarTypes &variableTypes,
            FuncRetTypes &functionReturnTypes)
      : functions(functions), functionReturnTypes(functionReturnTypes),
        variableTypes(variableTypes) {}
  void typecheck(const PExpr &root);
};

#endif
