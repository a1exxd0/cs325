#pragma once

#include <string>
#include <tokens/tokens.h>

namespace mccomp {

class Lexer {
  FILE *pFile;
  std::string globalLexeme;
  int lineNo, columnNo;
  std::string fileName;

public:
  Lexer(const char *fileName);
  ~Lexer();
  auto returnTok(std::string lexVal, TokenType tokType) -> Token;

  // Read file line by line -- or look for \n and if found add 1 to line number
  // and reset column number to 0
  /// gettok - Return the next token from standard input.
  auto gettok() -> Token;
};

}; // namespace mccomp
