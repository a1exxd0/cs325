#pragma once

#include <string>
#include <tokens/tokens.h>

namespace mccomp {

class Lexer {
  FILE *pFile;
  std::string globalLexeme;
  int lineNo, columnNo;
  int lastChar = ' ', nextChar = ' ';
  std::string fileName;

public:
  Lexer(const std::string &programName, const char *fileName);
  ~Lexer();

  // Read file line by line -- or look for \n and if found add 1 to line number
  // and reset column number to 0
  auto getToken() -> Token;

  // Peek next token
  auto peekToken() const -> Token;

  // Peek forwards n tokens (default 1), minimum 1.
  auto peekToken(std::size_t n) const -> Token;

  auto getFileName() const -> std::string_view { return fileName; }
};

}; // namespace mccomp
