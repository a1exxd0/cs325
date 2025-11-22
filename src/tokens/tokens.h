#pragma once

#include <cassert>
#include <error/error.h>
#include <fmt/format.h>
#include <llvm/ADT/StringExtras.h>
#include <string>
#include <string_view>
#include <tl/expected.hpp>
#include <variant>

namespace mccomp {

// The lexer returns one of these for known things.
enum class TokenType {

  IDENT = -1,        // [a-zA-Z_][a-zA-Z_0-9]*
  ASSIGN = int('='), // '='

  // delimiters
  LBRA = int('{'),  // left brace
  RBRA = int('}'),  // right brace
  LPAR = int('('),  // left parenthesis
  LBOX = int('['),  // left bracket
  RBOX = int(']'),  // right bracket
  RPAR = int(')'),  // right parenthesis
  SC = int(';'),    // semicolon
  COMMA = int(','), // comma

  // types
  INT_TOK = -2,   // "int"
  VOID_TOK = -3,  // "void"
  FLOAT_TOK = -4, // "float"
  BOOL_TOK = -5,  // "bool"

  // keywords
  EXTERN = -6,  // "extern"
  IF = -7,      // "if"
  ELSE = -8,    // "else"
  WHILE = -9,   // "while"
  RETURN = -10, // "return"
  TRUE = -12,   // "true"
  FALSE = -13,  // "false"

  // literals
  INT_LIT = -14,   // [0-9]+
  FLOAT_LIT = -15, // [0-9]+.[0-9]+
  BOOL_LIT = -16,  // "true" or "false" key words

  // logical operators
  AND = -17, // "&&"
  OR = -18,  // "||"

  // operators
  PLUS = int('+'),    // addition or unary plus
  MINUS = int('-'),   // substraction or unary negative
  ASTERIX = int('*'), // multiplication
  DIV = int('/'),     // division
  MOD = int('%'),     // modular
  NOT = int('!'),     // unary negation

  // comparison operators
  EQ = -19,      // equal
  NE = -20,      // not equal
  LE = -21,      // less than or equal to
  LT = int('<'), // less than
  GE = -23,      // greater than or equal to
  GT = int('>'), // greater than

  // special tokens
  EOF_TOK = 0, // signal end of file

  // invalid
  INVALID = -100, // signal invalid token
  SINGLE_AMPERSAND = int('&'),
  SINGLE_PIPE = int('|'),
};

const char *to_string(TokenType t);

// Token class is used to keep track of information about a token
class Token {
  using TokenVariant = std::variant<std::monostate, int, double, bool>;

  std::string lexeme;
  TokenVariant value;
  TokenType tokenType;
  int lineNo;
  int columnNo;

  Token();

public:
  // Constructs a Token and converts the lexeme to the appropriate internal
  // type. For IDENT tokens, stores lexeme as a string. For INT_LIT tokens,
  // converts lexeme to int (must be valid integer format). For FLOAT_LIT
  // tokens, converts lexeme to float (must be valid float format). For BOOL_LIT
  // tokens, converts lexeme to bool (must be "true" or "false"). For other
  // token types, stores lexeme as-is.
  //
  // Assumptions:
  // - lexeme must not be empty for IDENT, INT_LIT, and FLOAT_LIT tokens
  // - lexeme must be "true" or "false" for BOOL_LIT tokens
  // - Program exits with code 2 if type conversion fails
  static auto buildToken(TokenType tokenType, std::string_view fileName,
                         std::size_t lineNo, std::size_t columnNo,
                         std::string &&lexeme) -> Token;

  // Asserts the value is an identifier, and returns the value
  auto asIdent() const -> const std::string &;

  // Asserts value is an int literal, then returns value
  auto asInt() const -> int;

  // Asserts value is a float literal then returns value
  auto asFloat() const -> double;

  // Asserts value is a bool literal then returns value
  auto asBool() const -> bool;

  auto getLexeme() const -> const std::string &;
  auto getLineNo() const noexcept -> int;
  auto getColumnNo() const noexcept -> int;
  auto getTokenType() const noexcept -> TokenType;

  template <typename T> auto in(T &&container) const noexcept -> bool {
    return std::find(std::begin(container), std::end(container),
                     this->tokenType) != std::end(container);
  }

  friend auto operator<<(std::ostream &, const Token &) -> std::ostream &;
};

inline auto operator<<(std::ostream &os, const Token &token) -> std::ostream & {
  auto str =
      fmt::format("{} :: {}:{}", token.lexeme, token.lineNo, token.columnNo);

  return os << str;
}

}; // namespace mccomp
