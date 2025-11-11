#include <iostream>
#include <optional>
#include <tokens/tokens.h>
#include <variant>

namespace mccomp {

Token::Token(TokenType tokenType, std::string_view fileName, std::size_t lineNo,
             std::size_t columnNo, std::string &&lexeme)
    : tokenType(tokenType), lineNo(lineNo), columnNo(columnNo) {
  switch (this->tokenType) {
  case TokenType::IDENT:
    assert(!lexeme.empty());
    this->lexeme = std::move(lexeme);
    return;
  case TokenType::INT_LIT:
    assert(!lexeme.empty());
    int lexemeAsInt;

    if (llvm::to_integer<int>(lexeme, lexemeAsInt)) {
      this->lexeme = lexemeAsInt;
    } else {
      auto error =
          ClangError(ClangErrorSeverity::ERROR, fileName, lineNo, columnNo,
                     "failed to convert integer lexeme to int");
      std::cerr << to_string(error) << std::endl;
      exit(2);
    }
    return;
  case TokenType::FLOAT_LIT:
    assert(!lexeme.empty());
    double lexemeAsFloat;

    if (llvm::to_float(lexeme, lexemeAsFloat)) {
      this->lexeme = lexemeAsFloat;
    } else {
      auto error =
          ClangError(ClangErrorSeverity::ERROR, fileName, lineNo, columnNo,
                     "failed to convert float lexeme to float");
      std::cerr << to_string(error) << std::endl;
      exit(2);
    }
    return;
  case TokenType::BOOL_LIT:
    if (lexeme == "true") {
      this->lexeme = true;
    } else if (lexeme == "false") {
      this->lexeme = false;
    } else {
      auto error =
          ClangError(ClangErrorSeverity::ERROR, fileName, lineNo, columnNo,
                     "failed to convert bool lexeme to bool");
      std::cerr << to_string(error) << std::endl;
      exit(2);
    }
    return;
  default:
    this->lexeme = lexeme;
  }
}

auto Token::getInt() const -> std::optional<int> {
  if (this->tokenType != TokenType::INT_LIT) {
    return std::nullopt;
  }

  return std::get<int>(this->lexeme);
}

auto Token::getFloat() const -> std::optional<double> {
  if (this->tokenType != TokenType::FLOAT_LIT) {
    return std::nullopt;
  }

  return std::get<double>(this->lexeme);
}

auto Token::getBool() const -> std::optional<bool> {
  if (this->tokenType != TokenType::BOOL_LIT) {
    return std::nullopt;
  }

  return std::get<bool>(this->lexeme);
}

auto Token::getIdent() const -> std::optional<std::string> {
  if (this->tokenType != TokenType::IDENT) {
    return std::nullopt;
  }

  return std::get<std::string>(this->lexeme);
}

auto Token::getLexeme() const -> std::optional<std::string> {
  if (!std::holds_alternative<std::string>(this->lexeme)) {
    return std::nullopt;
  }

  return std::get<std::string>(this->lexeme);
}

auto Token::getLineNo() const noexcept -> int { return this->lineNo; }
auto Token::getColumnNo() const noexcept -> int { return this->columnNo; }
auto Token::getTokenType() const noexcept -> TokenType {
  return this->tokenType;
}

}; // namespace mccomp
