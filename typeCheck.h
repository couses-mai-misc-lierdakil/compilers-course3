#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "parserBase.h"

class TypeCheck {
  void visit(const PExpr &root);
public:
  void typecheck(const PExpr &root);
};

#endif
