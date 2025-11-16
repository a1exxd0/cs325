#include <ast/ast.h>
#include <deque>
#include <lexer/lexer.h>
#include <tokens/tokens.h>

namespace mccomp {

class Parser {
  /// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the
  /// current token the parser is looking at.  getNextToken reads another token
  /// from the lexer and updates CurTok with its results.
  std::optional<Token> currentToken;
  std::deque<Token> tokenBuffer;

public:
  // Advance the state of the parser w.r.t. the lexer
  auto getNextToken(Lexer &lexer) -> Token {
    if (tokenBuffer.size() == 0)
      tokenBuffer.push_back(lexer.getToken());

    auto temp = tokenBuffer.front();
    tokenBuffer.pop_front();
    currentToken = temp;

    return temp;
  }

  auto peekNextToken(const Lexer &lexer) const -> Token {
    auto token =
        (tokenBuffer.size() == 0) ? lexer.peekToken() : tokenBuffer.front();

    return token;
  }

  // Peek forward n tokens, default 1
  auto peekNextToken(const Lexer &lexer, std::size_t n) const -> Token {
    auto token =
        (tokenBuffer.size() >= n) ? tokenBuffer[n - 1] : lexer.peekToken(n);

    return token;
  }

  auto parseProgram(Lexer &lexer) -> void { return; }
};
} // namespace mccomp
