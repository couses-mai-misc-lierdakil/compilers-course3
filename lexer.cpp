#include "lexer.h"
#include <stdexcept>

int transTable(char c, int st) {
  switch (st) {
  case 0:
    if (c >= '0' && c <= '9')
      return 1;
    else if (c == '+' || c == '*' || c == '/' || c == '-' || c == '^')
      return 6;
    else if ((c >= 'a' && c <= 'z') || c == '_')
      return 7;
    else if (c == '(')
      return 8;
    else if (c == ')')
      return 9;
    else if (c == ',')
      return 10;
    else if (c == ' ' || c == '\t' || c == '\n' || c == '\r')
      return 0;
    break;
  case 1:
    if (c >= '0' && c <= '9')
      return 1;
    else if (c == '.')
      return 2;
    else if (c == 'e' || c == 'E')
      return 3;
    break;
  case 2:
    if (c >= '0' && c <= '9')
      return 2;
    else if (c == 'e' || c == 'E')
      return 3;
    break;
  case 3:
    if (c >= '0' && c <= '9')
      return 5;
    else if (c == '+' || c == '-')
      return 4;
    break;
  case 4:
    if (c >= '0' && c <= '9')
      return 5;
    break;
  case 5:
    if (c >= '0' && c <= '9')
      return 5;
    break;
  case 6:
    break;
  case 7:
    if ((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_')
      return 7;
    break;
  }
  return -1;
}

Lexer::Lexer(const std::string &input) : input(input), curChIx(0) {}

Token Lexer::getNextToken() {
  std::string buf;
  std::size_t lastAccChIx = curChIx;
  int curSt = 0;
  Token resultToken{TokenType::Invalid};
  while (curSt != -1) {
    if (curSt == 1 || curSt == 2 || curSt == 5) {
      resultToken.type = TokenType::Number;
      resultToken.attribute = buf;
      lastAccChIx = curChIx;
    } else if (curSt == 6) {
      resultToken.type = TokenType::Operator;
      resultToken.attribute = buf;
      lastAccChIx = curChIx;
    } else if (curSt == 7) {
      resultToken.type = TokenType::Id;
      resultToken.attribute = buf;
      lastAccChIx = curChIx;
    } else if (curSt == 8) {
      resultToken.type = TokenType::LParen;
      resultToken.attribute = buf;
      lastAccChIx = curChIx;
    } else if (curSt == 9) {
      resultToken.type = TokenType::RParen;
      resultToken.attribute = buf;
      lastAccChIx = curChIx;
    } else if (curSt == 10) {
      resultToken.type = TokenType::Comma;
      resultToken.attribute = buf;
      lastAccChIx = curChIx;
    }
    if (curChIx >= input.size())
      break;
    char curCh = input[curChIx];
    curChIx += 1;
    curSt = transTable(curCh, curSt);
    if (curSt > 0) // пропуск пробельных символов
      buf += curCh;
  }
  // if (resultToken.type == TokenType::Invalid)
  //   throw new std::runtime_error("Invalid token in input " + input);
  curChIx = lastAccChIx;
  return resultToken;
}
