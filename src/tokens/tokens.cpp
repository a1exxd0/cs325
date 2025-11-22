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

const char *to_string(TokenType t) {
  switch (t) {
  case TokenType::IDENT:
    return "IDENT";
  case TokenType::ASSIGN:
    return "ASSIGN";
  case TokenType::LBRA:
    return "LBRA";
  case TokenType::RBRA:
    return "RBRA";
  case TokenType::LPAR:
    return "LPAR";
  case TokenType::LBOX:
    return "LBOX";
  case TokenType::RBOX:
    return "RBOX";
  case TokenType::RPAR:
    return "RPAR";
  case TokenType::SC:
    return "SC";
  case TokenType::COMMA:
    return "COMMA";

  case TokenType::INT_TOK:
    return "INT_TOK";
  case TokenType::VOID_TOK:
    return "VOID_TOK";
  case TokenType::FLOAT_TOK:
    return "FLOAT_TOK";
  case TokenType::BOOL_TOK:
    return "BOOL_TOK";

  case TokenType::EXTERN:
    return "EXTERN";
  case TokenType::IF:
    return "IF";
  case TokenType::ELSE:
    return "ELSE";
  case TokenType::WHILE:
    return "WHILE";
  case TokenType::RETURN:
    return "RETURN";
  case TokenType::TRUE:
    return "TRUE";
  case TokenType::FALSE:
    return "FALSE";

  case TokenType::INT_LIT:
    return "INT_LIT";
  case TokenType::FLOAT_LIT:
    return "FLOAT_LIT";
  case TokenType::BOOL_LIT:
    return "BOOL_LIT";

  case TokenType::AND:
    return "AND";
  case TokenType::OR:
    return "OR";

  case TokenType::PLUS:
    return "PLUS";
  case TokenType::MINUS:
    return "MINUS";
  case TokenType::ASTERIX:
    return "ASTERIX";
  case TokenType::DIV:
    return "DIV";
  case TokenType::MOD:
    return "MOD";
  case TokenType::NOT:
    return "NOT";

  case TokenType::EQ:
    return "EQ";
  case TokenType::NE:
    return "NE";
  case TokenType::LE:
    return "LE";
  case TokenType::LT:
    return "LT";
  case TokenType::GE:
    return "GE";
  case TokenType::GT:
    return "GT";

  case TokenType::EOF_TOK:
    return "EOF_TOK";
  case TokenType::INVALID:
    return "INVALID";
  case TokenType::SINGLE_AMPERSAND:
    return "SINGLE_AMPERSAND";
  case TokenType::SINGLE_PIPE:
    return "SINGLE_PIPE";

  default:
    return "UNKNOWN";
  }
}

}; // namespace mccomp
