#include <cstdio>
#include <error/error.h>
#include <fmt/format.h>
#include <lexer/lexer.h>
#include <string>
#include <tokens/tokens.h>

// Construct a token from its lexed value and specified type.
// Nothing checked here directly, all work is done in Token
// type constructor.
auto returnToken(int lineNo, int columnNo, const std::string &fileName,
                 std::string lexVal, mccomp::TokenType tokType)
    -> mccomp::Token {
  auto token = mccomp::Token::buildToken(tokType, fileName, lineNo, columnNo,
                                         std::move(lexVal));
  return token;
}

inline auto tokenize(FILE *pFile, int &lastChar, int &nextChar, int &lineNo,
                     int &columnNo, std::string &globalLexeme,
                     const std::string &fileName) -> mccomp::Token {
  // Skip any whitespace.
  while (isspace(lastChar)) {
    if (lastChar == '\n' || lastChar == '\r') {
      lineNo++;
      columnNo = 0;
    }
    lastChar = getc(pFile);
    columnNo++;
  }

  if (isalpha(lastChar) ||
      (lastChar == '_')) { // identifier: [a-zA-Z_][a-zA-Z_0-9]*
    globalLexeme = lastChar;
    columnNo++;

    while (isalnum((lastChar = getc(pFile))) || (lastChar == '_')) {
      globalLexeme += lastChar;
      columnNo++;
    }

    if (globalLexeme == "int")
      return returnToken(lineNo, columnNo, fileName, "int",
                         mccomp::TokenType::INT_TOK);
    if (globalLexeme == "bool")
      return returnToken(lineNo, columnNo, fileName, "bool",
                         mccomp::TokenType::BOOL_TOK);
    if (globalLexeme == "float")
      return returnToken(lineNo, columnNo, fileName, "float",
                         mccomp::TokenType::FLOAT_TOK);
    if (globalLexeme == "void")
      return returnToken(lineNo, columnNo, fileName, "void",
                         mccomp::TokenType::VOID_TOK);
    if (globalLexeme == "bool")
      return returnToken(lineNo, columnNo, fileName, "bool",
                         mccomp::TokenType::BOOL_TOK);
    if (globalLexeme == "extern")
      return returnToken(lineNo, columnNo, fileName, "extern",
                         mccomp::TokenType::EXTERN);
    if (globalLexeme == "if")
      return returnToken(lineNo, columnNo, fileName, "if",
                         mccomp::TokenType::IF);
    if (globalLexeme == "else")
      return returnToken(lineNo, columnNo, fileName, "else",
                         mccomp::TokenType::ELSE);
    if (globalLexeme == "while")
      return returnToken(lineNo, columnNo, fileName, "while",
                         mccomp::TokenType::WHILE);
    if (globalLexeme == "return")
      return returnToken(lineNo, columnNo, fileName, "return",
                         mccomp::TokenType::RETURN);
    if (globalLexeme == "true") {
      //   BoolVal = true;
      return returnToken(lineNo, columnNo, fileName, "true",
                         mccomp::TokenType::BOOL_LIT);
    }
    if (globalLexeme == "false") {
      //   BoolVal = false;
      return returnToken(lineNo, columnNo, fileName, "false",
                         mccomp::TokenType::BOOL_LIT);
    }
    return returnToken(lineNo, columnNo, fileName, globalLexeme.c_str(),
                       mccomp::TokenType::IDENT);
  }

  if (lastChar == '=') {
    nextChar = getc(pFile);
    if (nextChar == '=') { // EQ: ==
      lastChar = getc(pFile);
      columnNo += 2;
      return returnToken(lineNo, columnNo, fileName,
                         "==", mccomp::TokenType::EQ);
    } else {
      lastChar = nextChar;
      columnNo++;
      return returnToken(lineNo, columnNo, fileName, "=",
                         mccomp::TokenType::ASSIGN);
    }
  }

  if (lastChar == '{') {
    lastChar = getc(pFile);
    columnNo++;
    return returnToken(lineNo, columnNo, fileName, "{",
                       mccomp::TokenType::LBRA);
  }
  if (lastChar == '}') {
    lastChar = getc(pFile);
    columnNo++;
    return returnToken(lineNo, columnNo, fileName, "}",
                       mccomp::TokenType::RBRA);
  }
  if (lastChar == '(') {
    lastChar = getc(pFile);
    columnNo++;
    return returnToken(lineNo, columnNo, fileName, "(",
                       mccomp::TokenType::LPAR);
  }
  if (lastChar == ')') {
    lastChar = getc(pFile);
    columnNo++;
    return returnToken(lineNo, columnNo, fileName, ")",
                       mccomp::TokenType::RPAR);
  }
  if (lastChar == ';') {
    lastChar = getc(pFile);
    columnNo++;
    return returnToken(lineNo, columnNo, fileName, ";", mccomp::TokenType::SC);
  }
  if (lastChar == ',') {
    lastChar = getc(pFile);
    columnNo++;
    return returnToken(lineNo, columnNo, fileName, ",",
                       mccomp::TokenType::COMMA);
  }

  if (isdigit(lastChar) || lastChar == '.') { // Number: [0-9]+.
    std::string NumStr;

    if (lastChar == '.') { // Floatingpoint Number: .[0-9]+
      do {
        NumStr += lastChar;
        lastChar = getc(pFile);
        columnNo++;
      } while (isdigit(lastChar));

      //   FloatVal = strtof(NumStr.c_str(), nullptr);
      return returnToken(lineNo, columnNo, fileName, NumStr,
                         mccomp::TokenType::FLOAT_LIT);
    } else {
      do { // Start of Number: [0-9]+
        NumStr += lastChar;
        lastChar = getc(pFile);
        columnNo++;
      } while (isdigit(lastChar));

      if (lastChar == '.') { // Floatingpoint Number: [0-9]+.[0-9]+)
        do {
          NumStr += lastChar;
          lastChar = getc(pFile);
          columnNo++;
        } while (isdigit(lastChar));

        // FloatVal = strtof(NumStr.c_str(), nullptr);
        return returnToken(lineNo, columnNo, fileName, NumStr,
                           mccomp::TokenType::FLOAT_LIT);
      } else { // Integer : [0-9]+
        // IntVal = strtod(NumStr.c_str(), nullptr);
        return returnToken(lineNo, columnNo, fileName, NumStr,
                           mccomp::TokenType::INT_LIT);
      }
    }
  }

  if (lastChar == '&') {
    nextChar = getc(pFile);
    if (nextChar == '&') { // AND: &&
      lastChar = getc(pFile);
      columnNo += 2;
      return returnToken(lineNo, columnNo, fileName, "&&",
                         mccomp::TokenType::AND);
    } else {
      lastChar = nextChar;
      columnNo++;
      return returnToken(lineNo, columnNo, fileName, "&",
                         mccomp::TokenType::INVALID);
    }
  }

  if (lastChar == '|') {
    nextChar = getc(pFile);
    if (nextChar == '|') { // OR: ||
      lastChar = getc(pFile);
      columnNo += 2;
      return returnToken(lineNo, columnNo, fileName, "||",
                         mccomp::TokenType::OR);
    } else {
      lastChar = nextChar;
      columnNo++;
      return returnToken(lineNo, columnNo, fileName, "|",
                         mccomp::TokenType::INVALID);
    }
  }

  if (lastChar == '!') {
    nextChar = getc(pFile);
    if (nextChar == '=') { // NE: !=
      lastChar = getc(pFile);
      columnNo += 2;
      return returnToken(lineNo, columnNo, fileName,
                         "!=", mccomp::TokenType::NE);
    } else {
      lastChar = nextChar;
      columnNo++;
      return returnToken(lineNo, columnNo, fileName, "!",
                         mccomp::TokenType::NOT);
      ;
    }
  }

  if (lastChar == '<') {
    nextChar = getc(pFile);
    if (nextChar == '=') { // LE: <=
      lastChar = getc(pFile);
      columnNo += 2;
      return returnToken(lineNo, columnNo, fileName,
                         "<=", mccomp::TokenType::LE);
    } else {
      lastChar = nextChar;
      columnNo++;
      return returnToken(lineNo, columnNo, fileName, "<",
                         mccomp::TokenType::LT);
    }
  }

  if (lastChar == '>') {
    nextChar = getc(pFile);
    if (nextChar == '=') { // GE: >=
      lastChar = getc(pFile);
      columnNo += 2;
      return returnToken(lineNo, columnNo, fileName,
                         ">=", mccomp::TokenType::GE);
    } else {
      lastChar = nextChar;
      columnNo++;
      return returnToken(lineNo, columnNo, fileName, ">",
                         mccomp::TokenType::GT);
    }
  }

  if (lastChar == '/') { // could be division or could be the start of a comment
    lastChar = getc(pFile);
    columnNo++;
    if (lastChar == '/') { // definitely a comment
      do {
        lastChar = getc(pFile);
        columnNo++;
      } while (lastChar != EOF && lastChar != '\n' && lastChar != '\r');

      if (lastChar != EOF)
        return tokenize(pFile, lastChar, nextChar, lineNo, columnNo,
                        globalLexeme, fileName);
    } else
      return returnToken(lineNo, columnNo, fileName, "/",
                         mccomp::TokenType::DIV);
  }

  // Check for end of file.  Don't eat the EOF.
  if (lastChar == EOF) {
    columnNo++;
    return returnToken(lineNo, columnNo, fileName, "0",
                       mccomp::TokenType::EOF_TOK);
  }

  // Otherwise, just return the character and invalid identifier.
  auto thisChar = lastChar;
  std::string s(1, thisChar);
  lastChar = getc(pFile);
  columnNo++;
  return returnToken(lineNo, columnNo, fileName, s, mccomp::TokenType::INVALID);
}

namespace mccomp {

Lexer::Lexer(const std::string &programName, const char *fileName)
    : globalLexeme(), lineNo(0), columnNo(0), fileName(fileName) {
  auto file = std::fopen(fileName, "r");
  if (file == nullptr) {
    fmt::println(
        stderr, "{}{}:{} {}error:{} {}no such file or directory: \'{}\'",
        text_colors::BOLD, programName, text_colors::RESET, text_colors::RED,
        text_colors::RESET, text_colors::BOLD, fileName);
    exit(3);
  } else {
    this->pFile = file;
  }
}

Lexer::~Lexer() { std::fclose(this->pFile); }

// Read file line by line -- or look for \n and if found add 1 to line number
// and reset column number to 0
/// gettok - Return the next token from standard input.
auto Lexer::getToken() -> Token {
  return tokenize(this->pFile, this->lastChar, this->nextChar, this->lineNo,
                  this->columnNo, this->globalLexeme, this->fileName);
}

auto Lexer::peekToken() const -> Token { return this->peekToken(1); }

auto Lexer::peekToken(std::size_t n) const -> Token {
  auto savedFilePosition = ftell(this->pFile);
  auto tempLastChar = this->lastChar;
  auto tempNextChar = this->nextChar;
  auto tempLineNo = this->lineNo;
  auto tempColumnNo = this->columnNo;
  auto tempLexeme = this->globalLexeme;

  auto token = tokenize(this->pFile, tempLastChar, tempNextChar, tempLineNo,
                        tempColumnNo, tempLexeme, this->fileName);
  for (decltype(n) i = 1; i < n; i++) {
    if (token.getTokenType() == TokenType::EOF_TOK) {
      return token;
    }

    token = tokenize(this->pFile, tempLastChar, tempNextChar, tempLineNo,
                     tempColumnNo, tempLexeme, this->fileName);
  }

  fseek(this->pFile, savedFilePosition, SEEK_SET);

  return token;
}

}; // namespace mccomp
