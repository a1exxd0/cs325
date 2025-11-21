#include "ast/_internal/ctx.h"
#include "error/error.h"
#include <ast/ast.h>
#include <deque>
#include <lexer/lexer.h>
#include <parser/_internal/util.h>
#include <tl/expected.hpp>
#include <tokens/tokens.h>
#include <vector>

namespace mccomp {

class Parser {
  /// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the
  /// current token the parser is looking at.  getNextToken reads another token
  /// from the lexer and updates CurTok with its results.
  std::optional<Token> currentToken;
  std::deque<Token> tokenBuffer;
  std::optional<Token> lastTokenConsumed;

public:
  // Advance the state of the parser w.r.t. the lexer
  auto getNextToken(Lexer &lexer) -> Token {
    if (tokenBuffer.size() == 0)
      tokenBuffer.push_back(lexer.getToken());

    auto temp = tokenBuffer.front();
    tokenBuffer.pop_front();
    lastTokenConsumed = currentToken;
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

  // program ::= [extern_list] decl_list
  auto parseProgram(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<TranslationUnit *, ClangError>;

  // Technically in C you can have extern variables, but
  // the grammar states only functions so we'll go with that.
  //
  // Do not expect the function declarations returned to have
  // a body.
  //
  // extern_list ::= extern+
  auto parseExternList(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::vector<FunctionDecl *>, ClangError>;

  // extern ::= "extern" type_spec IDENT "(" params ")" ";"
  auto parseExtern(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<FunctionDecl *, ClangError>;

  // Purposefully including a typed discrimination to be explicit
  // with how we parse.
  //
  // decl_list ::= decl+
  auto parseDeclList(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::vector<Decl *>, ClangError>;

  // decl ::= var_decl | fun_decl
  auto parseDecl(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<Decl *, ClangError>;

  // var_decl ::= num_type IDENT [dimensions] ";"
  //         | "bool" IDENT ";"
  auto parseVarDecl(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<VarDecl *, ClangError>;

  // Returns the token in tl::unexpected if we dont get
  // what we expect.
  //
  // type_spec ::= "void" | "bool" | num_type
  auto parseTypeSpec(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::pair<Token, Type *>, Token>;

  // num_type ::= "int" | "float"
  auto parseNumType(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::pair<Token, Type *>, Token>;

  // dimensions ::= "[" expr "]" ["[" expr "]" ["[" expr "]"]]
  template <bool IsDecl>
  auto parseDimensions(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::vector<Expr *>, ClangError> {
    return parseOptDimensions<false, IsDecl>(lexer, ctx);
  }

  // param_dimensions ::= "[" [expr] "]" ["[" expr "]" ["[" expr "]"]]
  auto parseParamDimensions(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::vector<Expr *>, ClangError>;

  // fun_decl ::= type_spec IDENT "(" params ")" block
  auto parseFunDecl(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<FunctionDecl *, ClangError>;

  // params ::= [param_list] | "void"
  auto parseParams(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::vector<ParmVarDecl *>, ClangError>;

  // param_list ::= param ("," param)*
  auto parseParamList(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::vector<ParmVarDecl *>, ClangError>;

  // param ::= num_type IDENT [param_dimensions] | "bool" IDENT
  auto parseParam(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<ParmVarDecl *, ClangError>;

  // block ::= "{" [local_decls] [stmt_list] "}"
  auto parseBlock(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<CompoundStmt *, ClangError>;

  // local_decls ::= local_decl+
  auto parseLocalDecls(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::vector<DeclStmt *>, ClangError>;

  // local_decl ::= num_type IDENT [dimensions] ";" | "bool" IDENT ";"
  auto parseLocalDecl(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<DeclStmt *, ClangError>;

  // stmt_list ::= stmt+
  auto parseStmtList(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::vector<ASTNode *>, ClangError>;

  // stmt ::= expr_stmt | block | if_stmt | while_stmt | return_stmt
  auto parseStmt(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<ASTNode *, ClangError>;

  // This could possibly be straight semicolons, will return a null pointer
  // with no issues.
  //
  // expr_stmt ::= [expr] ";"
  auto parseExprStmt(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<Expr *, ClangError>;

  // while_stmt ::= "while" "(" expr ")" stmt
  auto parseWhileStmt(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<WhileStmt *, ClangError>;

  // if_stmt ::= "if" "(" expr ")" block [else_stmt]
  auto parseIfStmt(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<IfStmt *, ClangError>;

  // Possibly null if it doesnt exist.
  //
  // else_stmt  ::= "else" block
  auto parseElseStmt(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<CompoundStmt *, ClangError>;

  // return_stmt ::= "return" ";" | "return" expr ";"
  auto parseReturnStmt(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<ReturnStmt *, ClangError>;

  // lvalue ::= IDENT [dimensions]
  auto parseLValue(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<Expr *, ClangError>;

  // expr ::= lvalue "=" expr | or_expr
  auto parseExpr(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<Expr *, ClangError>;

  // or_expr ::= and_expr ("||" and_expr)*
  auto parseOrExpr(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<Expr *, ClangError>;

  // and_expr ::= eq_expr ("&&" eq_expr)*
  auto parseAndExpr(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<Expr *, ClangError>;

  // eq_expr ::= rel_expr (("==" | "!=") rel_expr)*
  auto parseEqExpr(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<Expr *, ClangError>;

  // rel_expr ::= add_expr (("<=" | "<" | ">=" | ">") add_expr)*
  auto parseRelExpr(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<Expr *, ClangError>;

  // add_expr ::= mul_expr (("+" | "-") mul_expr)*
  auto parseAddExpr(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<Expr *, ClangError>;

  // mul_expr ::= unary_expr (("*" | "/" | "%") unary_expr)*
  auto parseMulExpr(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<Expr *, ClangError>;

  // unary_expr ::= "-" unary_expr | "!" unary_expr | primary
  auto parseUnaryExpr(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<Expr *, ClangError>;

  // primary ::= "(" expr ")"
  //         | IDENT "(" args ")"
  //         | lvalue
  //         | INT_LIT
  //         | FLOAT_LIT
  //         | BOOL_LIT
  auto parsePrimary(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<Expr *, ClangError>;

  // args ::= [arg_list]
  auto parseArgs(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::vector<Expr *>, ClangError>;

  // arg_list ::= expr ("," expr)*
  auto parseArgList(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::vector<Expr *>, ClangError>;

private:
  // templated error returns, must specify context of parse.
  //
  // IsDecl only matters if OptionalFirstExpr is false.
  template <bool OptionalFirstExpr, bool IsDecl>
  auto parseOptDimensions(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::vector<Expr *>, ClangError>;

  static auto buildBraceError(std::string_view fileName, const Token &lbr,
                              const Token &rbr, char l, char r) -> ClangError {
    auto error =
        ClangError(ClangErrorSeverity::ERROR, fileName, rbr.getLineNo(),
                   rbr.getColumnNo() + rbr.getLexeme().length(),
                   fmt::format("expected \'{}\'", r));

    auto note =
        ClangError(ClangErrorSeverity::NOTE, fileName, lbr.getLineNo(),
                   lbr.getColumnNo(), fmt::format("to match this \'{}\'", l));
    error.attached.push_back(std::move(note));

    return error;
  }

  // wow this is gross!
  template <typename C, typename F>
  static auto buildBinaryExprVectors(Parser &parser, Lexer &lexer,
                                     ASTContext &ctx, const C &validTokens,
                                     F func)
      -> tl::expected<std::pair<std::vector<Expr *>, std::vector<Token>>,
                      ClangError> {
    static_assert(
        std::is_invocable_r_v<tl::expected<Expr *, ClangError>, F, Parser &,
                              Lexer &, ASTContext &>,
        "Template parameter F must be callable with (Parser &, Lexer &, "
        "ASTContext &) -> tl::expected<Expr*, ClangError>");

    auto firstExpr = func(parser, lexer, ctx);
    if (!firstExpr) {
      return tl::unexpected(firstExpr.error());
    }

    auto exprs = std::vector<Expr *>{firstExpr.value()};
    auto toks = std::vector<Token>();
    auto lookahead = parser.peekNextToken(lexer);
    while (lookahead.in(validTokens)) {
      auto assign = parser.getNextToken(lexer);
      toks.push_back(std::move(assign));
      auto nextExpr = func(parser, lexer, ctx);
      if (!nextExpr) {
        return tl::unexpected(nextExpr.error());
      }

      exprs.push_back(nextExpr.value());
      lookahead = parser.peekNextToken(lexer);
    }

    return std::make_pair(exprs, toks);
  }
};

template <bool OptionalFirstExpr, bool IsDecl = true>
auto Parser::parseOptDimensions(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<std::vector<Expr *>, ClangError> {
  auto exprs = std::vector<Expr *>();
  auto lbox = this->getNextToken(lexer);
  if (lbox.getTokenType() != TokenType::LBOX) {
    tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(), lbox.getLineNo(),
        lbox.getColumnNo(),
        "unexpected entry into parse opt dimensions, check lookahead"));
  }

  auto nextToken = this->peekNextToken(lexer);
  if (nextToken.getTokenType() != TokenType::RBOX) {
    auto expr = parseExpr(lexer, ctx);
    if (!expr) {
      return tl::unexpected(expr.error());
    }

    exprs.push_back(expr.value());
  } else if (!OptionalFirstExpr && IsDecl) {
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(),
        this->lastTokenConsumed->getLineNo(),
        this->lastTokenConsumed->getColumnNo(),
        "definition of variable with array type needs an explicit size"));
  } else if (!OptionalFirstExpr && !IsDecl) {
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(), nextToken.getLineNo(),
        nextToken.getColumnNo(), "expected expression"));
  } else {
    // optional, use nullptr
    exprs.push_back(nullptr);
  }

  auto rbox = this->getNextToken(lexer);
  if (rbox.getTokenType() != TokenType::RBOX) {
    return tl::unexpected(this->buildBraceError(
        lexer.getFileName(), lbox, lastTokenConsumed.value(), '[', ']'));
  }

  lbox = this->peekNextToken(lexer);
  while (lbox.getTokenType() == TokenType::LBOX) {
    this->getNextToken(lexer);
    auto expr = parseExpr(lexer, ctx);
    if (!expr) {
      return tl::unexpected(expr.error());
    }
    exprs.push_back(expr.value());

    rbox = this->getNextToken(lexer);
    if (rbox.getTokenType() != TokenType::RBOX) {
      return tl::unexpected(buildBraceError(
          lexer.getFileName(), lbox, lastTokenConsumed.value(), '[', ']'));
    }

    lbox = this->peekNextToken(lexer);
  }

  if (exprs.size() > 3) {
    return tl::unexpected(ClangError(ClangErrorSeverity::ERROR,
                                     lexer.getFileName(), lbox.getLineNo(),
                                     lbox.getColumnNo(),
                                     "CS325 grammar does not allow for arrays "
                                     "of greater than 3 dimensions"));
  }

  return exprs;
}

} // namespace mccomp
