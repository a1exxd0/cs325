
#include "error/clang_error.h"
#include <cstdio>
#include <iostream>
#include <lexer/lexer.h>
#include <string>
#include <string_view>
#include <tokens/tokens.h>

namespace mccomp {

Lexer::Lexer(const char *fileName) : globalLexeme(), lineNo(0), columnNo(0) {
  auto file = std::fopen(fileName, "r");
  if (pFile == nullptr) {
    auto err = ClangError(ClangErrorSeverity::ERROR, fileName, 0, 0,
                          "couldn't open file");
    std::cerr << to_string(err) << std::endl;
    exit(3);
  } else {
    this->pFile = file;
  }
}

Lexer::~Lexer() { std::fclose(this->pFile); }

auto Lexer::returnTok(std::string lexVal, TokenType tokType) -> Token {
  auto return_tok = Token(tokType, fileName, lineNo,
                          columnNo - lexVal.length() - 1, std::move(lexVal));
  return return_tok;
}

// Read file line by line -- or look for \n and if found add 1 to line number
// and reset column number to 0
/// gettok - Return the next token from standard input.
auto Lexer::gettok() -> Token {

  static int LastChar = ' ';
  static int NextChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar)) {
    if (LastChar == '\n' || LastChar == '\r') {
      lineNo++;
      columnNo = 1;
    }
    LastChar = getc(pFile);
    columnNo++;
  }

  if (isalpha(LastChar) ||
      (LastChar == '_')) { // identifier: [a-zA-Z_][a-zA-Z_0-9]*
    globalLexeme = LastChar;
    columnNo++;

    while (isalnum((LastChar = getc(pFile))) || (LastChar == '_')) {
      globalLexeme += LastChar;
      columnNo++;
    }

    if (globalLexeme == "int")
      return returnTok("int", TokenType::INT_TOK);
    if (globalLexeme == "bool")
      return returnTok("bool", TokenType::BOOL_TOK);
    if (globalLexeme == "float")
      return returnTok("float", TokenType::FLOAT_TOK);
    if (globalLexeme == "void")
      return returnTok("void", TokenType::VOID_TOK);
    if (globalLexeme == "bool")
      return returnTok("bool", TokenType::BOOL_TOK);
    if (globalLexeme == "extern")
      return returnTok("extern", TokenType::EXTERN);
    if (globalLexeme == "if")
      return returnTok("if", TokenType::IF);
    if (globalLexeme == "else")
      return returnTok("else", TokenType::ELSE);
    if (globalLexeme == "while")
      return returnTok("while", TokenType::WHILE);
    if (globalLexeme == "return")
      return returnTok("return", TokenType::RETURN);
    if (globalLexeme == "true") {
      //   BoolVal = true;
      return returnTok("true", TokenType::BOOL_LIT);
    }
    if (globalLexeme == "false") {
      //   BoolVal = false;
      return returnTok("false", TokenType::BOOL_LIT);
    }
    return returnTok(globalLexeme.c_str(), TokenType::IDENT);
  }

  if (LastChar == '=') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // EQ: ==
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("==", TokenType::EQ);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("=", TokenType::ASSIGN);
    }
  }

  if (LastChar == '{') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("{", TokenType::LBRA);
  }
  if (LastChar == '}') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("}", TokenType::RBRA);
  }
  if (LastChar == '(') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("(", TokenType::LPAR);
  }
  if (LastChar == ')') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(")", TokenType::RPAR);
  }
  if (LastChar == ';') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(";", TokenType::SC);
  }
  if (LastChar == ',') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(",", TokenType::COMMA);
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9]+.
    std::string NumStr;

    if (LastChar == '.') { // Floatingpoint Number: .[0-9]+
      do {
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      //   FloatVal = strtof(NumStr.c_str(), nullptr);
      return returnTok(NumStr, TokenType::FLOAT_LIT);
    } else {
      do { // Start of Number: [0-9]+
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      if (LastChar == '.') { // Floatingpoint Number: [0-9]+.[0-9]+)
        do {
          NumStr += LastChar;
          LastChar = getc(pFile);
          columnNo++;
        } while (isdigit(LastChar));

        // FloatVal = strtof(NumStr.c_str(), nullptr);
        return returnTok(NumStr, TokenType::FLOAT_LIT);
      } else { // Integer : [0-9]+
        // IntVal = strtod(NumStr.c_str(), nullptr);
        return returnTok(NumStr, TokenType::INT_LIT);
      }
    }
  }

  if (LastChar == '&') {
    NextChar = getc(pFile);
    if (NextChar == '&') { // AND: &&
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("&&", TokenType::AND);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("&", TokenType::INVALID);
    }
  }

  if (LastChar == '|') {
    NextChar = getc(pFile);
    if (NextChar == '|') { // OR: ||
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("||", TokenType::OR);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("|", TokenType::INVALID);
    }
  }

  if (LastChar == '!') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // NE: !=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("!=", TokenType::NE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("!", TokenType::NOT);
      ;
    }
  }

  if (LastChar == '<') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // LE: <=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("<=", TokenType::LE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("<", TokenType::LT);
    }
  }

  if (LastChar == '>') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // GE: >=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok(">=", TokenType::GE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok(">", TokenType::GT);
    }
  }

  if (LastChar == '/') { // could be division or could be the start of a comment
    LastChar = getc(pFile);
    columnNo++;
    if (LastChar == '/') { // definitely a comment
      do {
        LastChar = getc(pFile);
        columnNo++;
      } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

      if (LastChar != EOF)
        return gettok();
    } else
      return returnTok("/", TokenType::DIV);
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF) {
    columnNo++;
    return returnTok("0", TokenType::EOF_TOK);
  }

  // Otherwise, just return the character and invalid identifier.
  int ThisChar = LastChar;
  std::string s(1, ThisChar);
  LastChar = getc(pFile);
  columnNo++;
  return returnTok(s, TokenType::INVALID);
}

}; // namespace mccomp
