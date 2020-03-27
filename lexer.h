#ifndef LEXER_H
#define LEXER_H
#include <string>

enum class TokenType { Number, Operator, Id, LParen, RParen, Comma, Invalid };

struct Token {
  TokenType type;
  std::string attribute;
};

int transTable(char c, int st);

class Lexer {
  std::string input;
  std::size_t curChIx;

public:
  Lexer(const std::string &input);
  Token getNextToken();
};
#endif
