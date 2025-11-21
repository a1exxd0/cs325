#include <iostream>
#include <tokens/tokens.h>

namespace mccomp {
Token::Token() {}
auto Token::buildToken(TokenType tokenType, std::string_view fileName,
                       std::size_t lineNo, std::size_t columnNo,
                       std::string &&lexeme) -> Token {
  Token token;
  token.tokenType = tokenType;
  token.lineNo = lineNo + 1;
  token.columnNo = columnNo - lexeme.size();
  token.lexeme = std::move(lexeme);

  switch (tokenType) {
  case TokenType::INT_LIT:
    int lexemeAsInt;
    if (!llvm::to_integer<int>(token.lexeme, lexemeAsInt)) {
      auto error = ClangError(
          ClangErrorSeverity::ERROR, fileName, lineNo, columnNo,
          "failed to convert integer lexeme to int (THIS IS A LEX ERROR)");
      std::cerr << to_string(error) << std::endl;
      exit(2);
    }

    token.value = lexemeAsInt;
    break;
  case TokenType::FLOAT_LIT:
    double lexemeAsFloat;
    if (!llvm::to_float(token.lexeme, lexemeAsFloat)) {
      auto error = ClangError(
          ClangErrorSeverity::ERROR, fileName, lineNo, columnNo,
          "failed to convert float lexeme to float (THIS IS A LEX ERROR)");
      std::cerr << to_string(error) << std::endl;
      exit(2);
    }
    token.value = lexemeAsFloat;
    break;
  case TokenType::BOOL_LIT:
    if (token.lexeme != "true" && token.lexeme != "false") {
      auto error =
          ClangError(ClangErrorSeverity::ERROR, fileName, lineNo, columnNo,
                     "failed to convert bool lexeme to bool");
      std::cerr << to_string(error) << std::endl;
      exit(2);
    }

    token.value = token.lexeme == "true";
    break;
  default:
  }

  return token;
}

auto Token::asInt() const -> int {
  assert(this->tokenType == TokenType::INT_LIT);
  return std::get<int>(this->value);
}

auto Token::asFloat() const -> double {
  assert(this->tokenType == TokenType::FLOAT_LIT);
  return std::get<double>(this->value);
}

auto Token::asBool() const -> bool {
  assert(this->tokenType == TokenType::BOOL_LIT);
  return std::get<bool>(this->value);
}

auto Token::asIdent() const -> const std::string & {
  assert(this->tokenType == TokenType::IDENT);
  return this->lexeme;
}

auto Token::getLexeme() const -> const std::string & { return this->lexeme; }
auto Token::getLineNo() const noexcept -> int { return this->lineNo; }
auto Token::getColumnNo() const noexcept -> int { return this->columnNo; }
auto Token::getTokenType() const noexcept -> TokenType {
  return this->tokenType;
}

}; // namespace mccomp
