#pragma once

#include <cassert>
#include <error/error.h>
#include <llvm/ADT/StringExtras.h>
#include <optional>
#include <string>
#include <string_view>
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

// Token class is used to keep track of information about a token
class Token {
  using TokenVariant = std::variant<std::string, int, double, bool>;

  TokenVariant lexeme = 0;
  TokenType tokenType;
  int lineNo;
  int columnNo;

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
  Token(TokenType tokenType, std::string_view fileName, std::size_t lineNo,
        std::size_t columnNo, std::string &&lexeme);

  // Returns nullopt if the token is not an identifier
  auto getIdent() const -> std::optional<std::string>;

  // Returns nullopt if the token is not an integer literal
  auto getInt() const -> std::optional<int>;

  // Returns nullopt if the token is not a float literal
  auto getFloat() const -> std::optional<double>;

  // Returns nullopt if the token is not a bool literal
  auto getBool() const -> std::optional<bool>;

  // Returns lexeme value if valid, else nullopt
  auto getLexeme() const -> std::optional<std::string>;

  auto getLineNo() const noexcept -> int;
  auto getColumnNo() const noexcept -> int;
  auto getTokenType() const noexcept -> TokenType;
};

}; // namespace mccomp
