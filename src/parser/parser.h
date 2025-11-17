#include "ast/_internal/ctx.h"
#include "ast/_internal/translation_unit.h"
#include <ast/ast.h>
#include <deque>
#include <lexer/lexer.h>
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

  // program ::= [extern_list] decl_list
  auto parseProgram(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<TranslationUnit, ClangError>;

  // Technically in C you can have extern variables, but
  // the grammar states only functions so we'll go with that.
  //
  // Do not expect the function declarations returned to have
  // a body.
  //
  // extern_list ::= extern+
  auto parseExternList(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::vector<FunctionDecl *>, ClangError>;

  auto parseExtern(Lexer &lexer, ASTContext ctx)
      -> tl::expected<FunctionDecl *, ClangError>;

  // Purposefully including a typed discrimination to be explicit
  // with how we parse.
  //
  // decl_list ::= decl+
  auto parseDeclList(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::vector<std::variant<FunctionDecl *, VarDecl *>>,
                      ClangError>;

  // decl ::= var_decl | fun_decl
  auto parseDecl(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::variant<FunctionDecl *, VarDecl *>, ClangError>;

  // var_decl ::= num_type IDENT [dimensions] ";"
  //         | "bool" IDENT ";"
  auto parseVarDecl(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<VarDecl *, ClangError>;

  // type_spec ::= "void" | "bool" | num_type
  auto parseTypeSpec(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<Token, ClangError>;

  // num_type ::= "int" | "float"
  auto parseNumType(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<Token, ClangError>;

  // dimensions ::= "[" expr "]" ["[" expr "]" ["[" expr "]"]]
  auto parseDimensions(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::vector<std::size_t>, ClangError>;

  // param_dimensions ::= "[" [expr] "]" ["[" expr "]" ["[" expr "]"]]
  auto parseParamDimensions(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<std::vector<std::size_t>, ClangError>;

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
      -> tl::expected<std::vector<Stmt *>, ClangError>;

  // stmt ::= expr_stmt | block | if_stmt | while_stmt | return_stmt
  auto parseStmt(Lexer &lexer, ASTContext &ctx)
      -> tl::expected<Stmt *, ClangError>;

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
};

} // namespace mccomp
