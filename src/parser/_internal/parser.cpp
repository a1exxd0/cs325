#include "ast/_internal/ast_node.h"
#include "ast/_internal/expr_node.h"
#include "error/error.h"
#include "parser/_internal/util.h"
#include <ast/ast.h>
#include <fmt/format.h>
#include <parser/parser.h>
#include <tl/expected.hpp>
#include <tokens/tokens.h>
#include <vector>

namespace mccomp {

// The returned source location is invalid for end column (but
// okay for rest).
//
// program ::= [extern_list] decl_list
auto Parser::parseProgram(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<TranslationUnit *, ClangError> {

  auto firstToken = this->peekNextToken(lexer);
  auto externList = std::vector<ASTNode *>();
  if (firstToken.getTokenType() == TokenType::EXTERN) {
    auto parsedExternList = this->parseExternList(lexer, ctx);

    if (!parsedExternList) {
      return tl::unexpected(parsedExternList.error());
    }

    externList = std::vector<ASTNode *>(parsedExternList.value().begin(),
                                        parsedExternList.value().end());
  }

  // offload first token check
  auto parsedDeclList = this->parseDeclList(lexer, ctx);
  if (!parsedDeclList) {
    return tl::unexpected(parsedDeclList.error());
  }

  auto declList = std::vector<ASTNode *>(parsedDeclList.value().begin(),
                                         parsedDeclList.value().end());
  auto start = (externList.size()) ? externList[0]->getLocation()
                                   : declList[0]->getLocation();

  return util::allocateNode<TranslationUnit>(
      ctx, std::move(externList), std::move(declList),
      SourceLocation(start.startLineNo, start.startColumnNo,
                     declList.back()->getLocation().endLineNo,
                     declList.back()->getLocation().endColumnNo,
                     lexer.getFileName()));
}

// Technically in C you can have extern variables, but
// the grammar states only functions so we'll go with that.
//
// Do not expect the function declarations returned to have
// a body.
//
// extern_list ::= extern+
auto Parser::parseExternList(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<std::vector<FunctionDecl *>, ClangError> {
  auto currToken = this->peekNextToken(lexer);
  auto externList = std::vector<FunctionDecl *>();
  while (currToken.getTokenType() == TokenType::EXTERN) {
    auto parsedExtern = this->parseExtern(lexer, ctx);
    if (!parsedExtern) {
      return tl::unexpected(parsedExtern.error());
    }

    externList.push_back(parsedExtern.value());
    currToken = this->peekNextToken(lexer);
  }

  if (externList.empty()) {
    return util::badParseCase();
  }

  return externList;
}

// extern ::= "extern" type_spec IDENT "(" params ")" ";"
auto Parser::parseExtern(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<FunctionDecl *, ClangError> {
  auto externToken = this->getNextToken(lexer);
  auto currToken = externToken;
  if (currToken.getTokenType() != TokenType::EXTERN) {
    return util::badParseCase();
  }

  auto typeSpec = this->parseTypeSpec(lexer, ctx);
  if (!typeSpec) {
    auto foundToken = typeSpec.error();

    auto error = [&lexer](const Token &tok) -> ClangError {
      if (tok.getTokenType() == TokenType::IDENT) {
        return ClangError(
            ClangErrorSeverity::ERROR, lexer.getFileName(), tok.getLineNo(),
            tok.getColumnNo(),
            fmt::format("unknown type name \'{}\'", tok.getLexeme()));
      } else {
        return ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                          tok.getLineNo(), tok.getColumnNo(),
                          fmt::format("expected type name"));
      }
    }(foundToken);

    return tl::unexpected(error);
  }

  auto ident = this->getNextToken(lexer);
  if (ident.getTokenType() != TokenType::IDENT) {
    return tl::unexpected(
        ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                   typeSpec.value().first.getLineNo(),
                   typeSpec.value().first.getColumnNo() +
                       typeSpec.value().first.getLexeme().size(),
                   fmt::format("expected identifier")));
  }

  currToken = this->getNextToken(lexer);
  if (currToken.getTokenType() != TokenType::LPAR) {
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(), ident.getLineNo(),
        ident.getColumnNo() + ident.getLexeme().size(), "(...)",
        fmt::format("expected parameter declarator")));
  }

  auto params = this->parseParams(lexer, ctx);
  if (!params)
    return tl::unexpected(params.error());

  auto rPar = this->getNextToken(lexer);
  if (rPar.getTokenType() != TokenType::RPAR) {
    auto error =
        buildBraceError(lexer.getFileName(), currToken, rPar, '(', ')');

    return tl::unexpected(error);
  }

  currToken = this->getNextToken(lexer);
  if (currToken.getTokenType() != TokenType::SC) {
    auto error = ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                            lastTokenConsumed->getLineNo(),
                            lastTokenConsumed->getColumnNo(), ";",
                            "expected \';\' after top level declarator");
    return tl::unexpected(error);
  }

  return util::allocateNode<FunctionDecl>(
      ctx, ident, typeSpec.value().second, params.value(), nullptr,
      SourceLocation(externToken.getLineNo(), externToken.getColumnNo(),
                     currToken.getLineNo(), currToken.getColumnNo(),
                     lexer.getFileName()));
}

// Purposefully including a typed discrimination to be explicit
// with how we parse.
//
// decl_list ::= decl+
auto Parser::parseDeclList(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<std::vector<Decl *>, ClangError> {
  auto firstDecl = parseDecl(lexer, ctx);
  if (!firstDecl) {
    return tl::unexpected(firstDecl.error());
  }

  auto decls = std::vector<Decl *>{firstDecl.value()};
  auto currToken = this->peekNextToken(lexer);
  // more offloading of tok start since this is a potential end point
  while (currToken.getTokenType() != TokenType::EOF_TOK) {
    auto decl = parseDecl(lexer, ctx);
    if (!decl) {
      return tl::unexpected(decl.error());
    }

    decls.push_back(decl.value());
    currToken = this->peekNextToken(lexer);
  }

  return decls;
}

// decl ::= var_decl | fun_decl
auto Parser::parseDecl(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<Decl *, ClangError> {
  auto firstToken = this->peekNextToken(lexer);
  if (firstToken.getTokenType() == TokenType::IDENT) {
    return tl::unexpected(
        ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                   firstToken.getLineNo(), firstToken.getColumnNo(), "int",
                   "type specifier missing, defaults to \'int\'; ISO C99 and "
                   "later do not"));
  } else if (firstToken.getTokenType() != TokenType::VOID_TOK &&
             firstToken.getTokenType() != TokenType::BOOL_TOK &&
             firstToken.getTokenType() != TokenType::INT_TOK &&
             firstToken.getTokenType() != TokenType::FLOAT_TOK) {
    this->getNextToken(lexer);
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(),
        lastTokenConsumed->getLineNo(), lastTokenConsumed->getColumnNo(),
        "expected function or variable declaration"));
  }

  auto differentiator = this->peekNextToken(lexer, 3);
  if (differentiator.getTokenType() == TokenType::LPAR ||
      firstToken.getTokenType() == TokenType::VOID_TOK) {
    return parseFunDecl(lexer, ctx);
  } else {
    return parseVarDecl(lexer, ctx);
  }
}

// MUST ASSERT first token.
//
// var_decl ::= num_type IDENT [dimensions] ";"
//         | "bool" IDENT ";"
auto Parser::parseVarDecl(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<VarDecl *, ClangError> {
  const auto firstToken = this->peekNextToken(lexer);
  if (firstToken.getTokenType() == TokenType::BOOL_TOK) {
    this->getNextToken(lexer);
    auto ident = this->getNextToken(lexer);
    if (ident.getTokenType() != TokenType::IDENT) {
      auto sc = this->peekNextToken(lexer);
      if (sc.getTokenType() == TokenType::SC) {
        return tl::unexpected(ClangError(
            ClangErrorSeverity::ERROR, lexer.getFileName(),
            firstToken.getLineNo(), firstToken.getColumnNo(),
            std::make_tuple(firstToken.getLineNo(), firstToken.getColumnNo(),
                            firstToken.getColumnNo() +
                                firstToken.getLexeme().size()),
            "declaration does not declare anything"));
      } else if (sc.getTokenType() == TokenType::EOF_TOK) {
        return tl::unexpected(
            ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                       firstToken.getLineNo(),
                       firstToken.getColumnNo() + firstToken.getLexeme().size(),
                       "expected identifier"));
      } else {
        return tl::unexpected(ClangError(
            ClangErrorSeverity::ERROR, lexer.getFileName(), ident.getLineNo(),
            ident.getColumnNo(), "expected identifier"));
      }
    }
  }

  auto numType = parseNumType(lexer, ctx);
  if (!numType) {
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(), firstToken.getLineNo(),
        firstToken.getColumnNo(),
        "should not reach here, error on someone calling parseVarDecl"));
  }

  auto ident = getNextToken(lexer);
  if (ident.getTokenType() != TokenType::IDENT) {
    if (ident.getTokenType() == TokenType::SC) {
      return tl::unexpected(ClangError(
          ClangErrorSeverity::ERROR, lexer.getFileName(),
          firstToken.getLineNo(), firstToken.getColumnNo(),
          std::make_tuple(firstToken.getLineNo(), firstToken.getColumnNo(),
                          firstToken.getColumnNo() +
                              firstToken.getLexeme().size()),
          "declaration does not declare anything"));
    } else if (ident.getTokenType() == TokenType::EOF_TOK) {
      return tl::unexpected(
          ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                     firstToken.getLineNo(),
                     firstToken.getColumnNo() + firstToken.getLexeme().size(),
                     "expected identifier"));
    } else {
      return tl::unexpected(ClangError(
          ClangErrorSeverity::ERROR, lexer.getFileName(), ident.getLineNo(),
          ident.getColumnNo(), "expected identifier"));
    }
  }

  auto optDims = std::optional<std::vector<Expr *>>();
  auto nextToken = this->peekNextToken(lexer);
  if (nextToken.getTokenType() == TokenType::LBOX) {
    auto dims = parseDimensions<true>(lexer, ctx);
    if (!dims) {
      return tl::unexpected(dims.error());
    }

    optDims = dims.value();
  }

  auto sc = this->getNextToken(lexer);
  if (sc.getTokenType() != TokenType::SC) {
    if (sc.getTokenType() == TokenType::EOF_TOK) {
      return tl::unexpected(
          ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                     lastTokenConsumed->getLineNo(),
                     lastTokenConsumed->getColumnNo() +
                         lastTokenConsumed->getLexeme().size(),
                     ";"
                     "expected \';\' after top level declarator"));
    } else {
      return tl::unexpected(ClangError(
          ClangErrorSeverity::ERROR, lexer.getFileName(), sc.getLineNo(),
          sc.getColumnNo(), ";", "expected \';\' after top level declarator"));
    }
  }

  auto type = (optDims.has_value())
                  ? ctx.getArrayType(numType->second, optDims.value())
                  : numType->second;
  return util::allocateNode<VarDecl>(
      ctx, ident, type, nullptr,
      SourceLocation(firstToken.getLineNo(), firstToken.getColumnNo(),
                     sc.getLineNo(), sc.getColumnNo(), lexer.getFileName()));
}

// type_spec ::= "void" | "bool" | num_type
auto Parser::parseTypeSpec(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<std::pair<Token, Type *>, Token> {
  auto currToken = this->peekNextToken(lexer);
  if (currToken.getTokenType() == TokenType::VOID_TOK) {
    this->getNextToken(lexer);
    return std::make_pair(currToken, ctx.getVoidType());
  } else if (currToken.getTokenType() == TokenType::BOOL_TOK) {
    this->getNextToken(lexer);
    return std::make_pair(currToken, ctx.getBoolType());
  } else {
    return parseNumType(lexer, ctx);
  }
}

// num_type ::= "int" | "float"
auto Parser::parseNumType(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<std::pair<Token, Type *>, Token> {
  auto currToken = this->getNextToken(lexer);
  if (currToken.getTokenType() == TokenType::INT_TOK) {
    return std::make_pair(currToken, ctx.getIntType());
  } else if (currToken.getTokenType() == TokenType::FLOAT_TOK) {
    return std::make_pair(currToken, ctx.getFloatType());
  } else {
    return tl::unexpected(currToken);
  }
}

// The first parameter is not doing anything.
//
// param_dimensions ::= "[" [expr] "]" ["[" expr "]" ["[" expr "]"]]
auto Parser::parseParamDimensions(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<std::vector<Expr *>, ClangError> {
  return parseOptDimensions<true>(lexer, ctx);
}

// MUST ASSERT FIRST TOKEN
//
// fun_decl ::= type_spec IDENT "(" params ")" block
auto Parser::parseFunDecl(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<FunctionDecl *, ClangError> {
  auto typeSpec = parseTypeSpec(lexer, ctx);
  if (!typeSpec) {
    return util::badParseCase();
  }

  auto ident = this->getNextToken(lexer);
  if (ident.getTokenType() != TokenType::IDENT) {
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(),
        typeSpec->first.getLineNo(),
        typeSpec->first.getColumnNo() + typeSpec->first.getLexeme().size(),
        "expected identifier"));
  }

  auto lpar = this->getNextToken(lexer);
  if (lpar.getTokenType() != TokenType::LPAR) {
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(), ident.getLineNo(),
        ident.getColumnNo() + ident.getLexeme().size(),
        "variable has incomplete type \'void\'"));
  }

  auto params = parseParams(lexer, ctx);
  if (!params) {
    return tl::unexpected(params.error());
  }

  auto rpar = this->getNextToken(lexer);
  if (rpar.getTokenType() != TokenType::RPAR) {
    return tl::unexpected(buildBraceError(lexer.getFileName(), lpar,
                                          lastTokenConsumed.value(), '(', ')'));
  }

  auto lbra = this->peekNextToken(lexer);
  if (lbra.getTokenType() != TokenType::LBRA) {
    return tl::unexpected(
        ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                   rpar.getLineNo(), rpar.getColumnNo(),
                   "expected function body after function declarator"));
  }

  auto block = this->parseBlock(lexer, ctx);
  if (!block) {
    return tl::unexpected(block.error());
  }

  return util::allocateNode<FunctionDecl>(
      ctx, ident, typeSpec->second, params.value(), block.value(),
      SourceLocation(typeSpec->first.getLineNo(), typeSpec->first.getColumnNo(),
                     block.value()->getLocation().endLineNo,
                     block.value()->getLocation().endColumnNo,
                     lexer.getFileName()));
}

// params ::= [param_list] | "void"
auto Parser::parseParams(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<std::vector<ParmVarDecl *>, ClangError> {
  auto currToken = this->peekNextToken(lexer);
  auto tokenAfter = this->peekNextToken(lexer, 2);
  if (currToken.getTokenType() == TokenType::VOID_TOK &&
      tokenAfter.getTokenType() == TokenType::IDENT) {
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(), tokenAfter.getLineNo(),
        tokenAfter.getColumnNo(), "argument may not have \'void\' type"));
  }

  if (currToken.getTokenType() == TokenType::VOID_TOK &&
      tokenAfter.getTokenType() == TokenType::RPAR) {
    this->getNextToken(lexer);
    return {};
  } else if (currToken.getTokenType() == TokenType::VOID_TOK) {
    this->getNextToken(lexer);
    assert(this->lastTokenConsumed.has_value());
    return tl::unexpected(buildBraceError(
        lexer.getFileName(), lastTokenConsumed.value(), currToken, '(', ')'));
  } else if (currToken.getTokenType() == TokenType::RPAR) {
    // follow case
    return {};
  }

  if (currToken.getTokenType() == TokenType::FLOAT_TOK ||
      currToken.getTokenType() == TokenType::BOOL_TOK ||
      currToken.getTokenType() == TokenType::INT_TOK) {
    return parseParamList(lexer, ctx);
  } else {
    if (currToken.getTokenType() == TokenType::IDENT) {
      if (tokenAfter.getTokenType() == TokenType::RPAR) {
        return tl::unexpected(
            ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                       currToken.getLineNo(), currToken.getColumnNo(),
                       "a parameter list without types is only allowed "
                       "in a function definition"));
      } else {
        return tl::unexpected(
            ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                       currToken.getLineNo(), currToken.getColumnNo(),
                       "type specifier missing, defaults to \'int\'; ISO C99 "
                       "and later do not support implicit int"));
      }
    } else if (tokenAfter.getTokenType() != TokenType::EOF_TOK) {
      return tl::unexpected(ClangError(
          ClangErrorSeverity::ERROR, lexer.getFileName(), currToken.getLineNo(),
          currToken.getColumnNo(), "expected parameter declarator"));
    } else {
      this->getNextToken(lexer);
      return tl::unexpected(ClangError(
          ClangErrorSeverity::ERROR, lexer.getFileName(),
          lastTokenConsumed->getLineNo(), lastTokenConsumed->getColumnNo(),
          "expected parameter declarator"));
    }
  }
}

// assume valid typename as first token
//
// param_list ::= param ("," param)*
auto Parser::parseParamList(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<std::vector<ParmVarDecl *>, ClangError> {
  auto firstParam = parseParam(lexer, ctx);
  if (!firstParam) {
    return tl::unexpected(std::move(firstParam).error());
  }

  auto paramList = std::vector<ParmVarDecl *>{firstParam.value()};
  auto currToken = this->peekNextToken(lexer);
  while (currToken.getTokenType() == TokenType::COMMA) {
    this->getNextToken(lexer);
    auto param = parseParam(lexer, ctx);
    if (!param) {
      return tl::unexpected(std::move(param).error());
    }

    paramList.push_back(param.value());
    currToken = this->peekNextToken(lexer);
  }

  return paramList;
}

// param ::= num_type IDENT [param_dimensions] | "bool" IDENT
auto Parser::parseParam(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<ParmVarDecl *, ClangError> {
  auto firstToken = this->peekNextToken(lexer);
  if (firstToken.getTokenType() == TokenType::BOOL_TOK) {
    this->getNextToken(lexer);
    auto ident = getNextToken(lexer);
    if (ident.getTokenType() != TokenType::IDENT) {
      return tl::unexpected(
          ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                     firstToken.getLineNo(), firstToken.getColumnNo(),
                     "expected identifier"));
    }

    return util::allocateNode<ParmVarDecl>(
        ctx, ident, ctx.getBoolType(),
        SourceLocation(firstToken.getLineNo(), firstToken.getColumnNo(),
                       ident.getLineNo(),
                       ident.getColumnNo() + ident.getLexeme().size(),
                       lexer.getFileName()));
  }

  auto numType = parseNumType(lexer, ctx);
  if (!numType) {
    return util::badParseCase();
  }

  auto ident = this->getNextToken(lexer);
  if (ident.getTokenType() != TokenType::IDENT) {
    return tl::unexpected(
        ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                   lastTokenConsumed->getLineNo(),
                   lastTokenConsumed->getColumnNo(), "expected identifier"));
  }

  auto lookahead = this->peekNextToken(lexer);
  auto optDims = std::optional<std::vector<Expr *>>();
  if (lookahead.getTokenType() == TokenType::LBOX) {
    auto dims = parseParamDimensions(lexer, ctx);
    if (!dims) {
      return tl::unexpected(dims.error());
    }

    optDims = dims.value();
  }

  lookahead = this->peekNextToken(lexer);
  /// TODO: during semantics we need to populate this type
  auto type = (optDims) ? ctx.getArrayType(numType->second, optDims.value())
                        : numType->second;

  return util::allocateNode<ParmVarDecl>(
      ctx, ident, type,
      SourceLocation(numType->first.getLineNo(), numType->first.getColumnNo(),
                     lookahead.getLineNo(), lookahead.getColumnNo(),
                     lexer.getFileName()));
}

// Verify that first token is indeed a LBRA before
// calling this function.
//
// block ::= "{" [local_decls] [stmt_list] "}"
auto Parser::parseBlock(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<CompoundStmt *, ClangError> {
  auto lbra = this->getNextToken(lexer);
  if (lbra.getTokenType() != TokenType::LBRA) {
    return util::badParseCase();
  }

  auto firstToken = this->peekNextToken(lexer);
  auto localDecls = std::vector<DeclStmt *>();
  if (firstToken.in(util::FIRST_local_decls)) {
    auto parsedDecls = parseLocalDecls(lexer, ctx);
    if (!parsedDecls) {
      return tl::unexpected(parsedDecls.error());
    }

    localDecls = std::move(parsedDecls.value());
  }

  firstToken = this->peekNextToken(lexer);
  auto stmtList = std::vector<ASTNode *>();
  if (firstToken.in(util::FIRST_stmt_list)) {
    auto parsedStmts = parseStmtList(lexer, ctx);
    if (!parsedStmts) {
      return tl::unexpected(parsedStmts.error());
    }

    stmtList = std::move(parsedStmts.value());
  }

  auto rbra = this->getNextToken(lexer);
  if (rbra.getTokenType() != TokenType::RBRA) {
    this->getNextToken(lexer);
    return tl::unexpected(buildBraceError(
        lexer.getFileName(), lbra, this->lastTokenConsumed.value(), '{', '}'));
  }

  return util::allocateNode<CompoundStmt>(
      ctx, localDecls, stmtList,
      SourceLocation(lbra.getLineNo(), lbra.getColumnNo(), rbra.getLineNo(),
                     rbra.getColumnNo(), lexer.getFileName()));
}

// assert first is correct before using
//
// local_decls ::= local_decl+
auto Parser::parseLocalDecls(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<std::vector<DeclStmt *>, ClangError> {
  auto decls = std::vector<DeclStmt *>();
  auto currToken = this->peekNextToken(lexer);
  while (std::find(util::FIRST_local_decl.begin(), util::FIRST_local_decl.end(),
                   currToken.getTokenType()) != util::FIRST_local_decl.end()) {
    auto localDecl = parseLocalDecl(lexer, ctx);
    if (!localDecl) {
      return tl::unexpected(localDecl.error());
    }

    decls.push_back(localDecl.value());
    currToken = this->peekNextToken(lexer);
  }

  if (decls.empty()) {
    return util::badParseCase();
  }

  return decls;
}

// assert first is correct before using
//
// local_decl ::= num_type IDENT [dimensions] ";" | "bool" IDENT ";"
auto Parser::parseLocalDecl(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<DeclStmt *, ClangError> {
  auto firstToken = this->peekNextToken(lexer);
  if (firstToken.getTokenType() == TokenType::BOOL_TOK) {
    this->getNextToken(lexer);
    auto ident = this->getNextToken(lexer);
    if (ident.getTokenType() != TokenType::IDENT) {
      auto sc = this->peekNextToken(lexer);
      if (sc.getTokenType() == TokenType::SC) {
        return tl::unexpected(ClangError(
            ClangErrorSeverity::ERROR, lexer.getFileName(),
            firstToken.getLineNo(), firstToken.getColumnNo(),
            std::make_tuple(firstToken.getLineNo(), firstToken.getColumnNo(),
                            firstToken.getColumnNo() +
                                firstToken.getLexeme().size()),
            "declaration does not declare anything"));
      } else if (sc.getTokenType() == TokenType::EOF_TOK) {
        return tl::unexpected(
            ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                       firstToken.getLineNo(),
                       firstToken.getColumnNo() + firstToken.getLexeme().size(),
                       "expected identifier"));
      } else {
        return tl::unexpected(ClangError(
            ClangErrorSeverity::ERROR, lexer.getFileName(), ident.getLineNo(),
            ident.getColumnNo(), "expected identifier"));
      }
    }
  }

  auto numType = parseNumType(lexer, ctx);
  if (!numType) {
    return util::badParseCase();
  }

  auto ident = getNextToken(lexer);
  if (ident.getTokenType() != TokenType::IDENT) {
    if (ident.getTokenType() == TokenType::SC) {
      return tl::unexpected(ClangError(
          ClangErrorSeverity::ERROR, lexer.getFileName(),
          firstToken.getLineNo(), firstToken.getColumnNo(),
          std::make_tuple(firstToken.getLineNo(), firstToken.getColumnNo(),
                          firstToken.getColumnNo() +
                              firstToken.getLexeme().size()),
          "declaration does not declare anything"));
    } else if (ident.getTokenType() == TokenType::EOF_TOK) {
      return tl::unexpected(
          ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                     firstToken.getLineNo(),
                     firstToken.getColumnNo() + firstToken.getLexeme().size(),
                     "expected identifier"));
    } else {
      return tl::unexpected(ClangError(
          ClangErrorSeverity::ERROR, lexer.getFileName(), ident.getLineNo(),
          ident.getColumnNo(), "expected identifier"));
    }
  }

  auto optDims = std::optional<std::vector<Expr *>>();
  auto nextToken = this->peekNextToken(lexer);
  if (nextToken.getTokenType() == TokenType::LBOX) {
    auto dims = parseDimensions<true>(lexer, ctx);
    if (!dims) {
      return tl::unexpected(dims.error());
    }

    optDims = dims.value();
  }

  auto sc = this->getNextToken(lexer);
  if (sc.getTokenType() != TokenType::SC) {
    if (sc.getTokenType() == TokenType::EOF_TOK) {
      return tl::unexpected(
          ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                     lastTokenConsumed->getLineNo(),
                     lastTokenConsumed->getColumnNo() +
                         lastTokenConsumed->getLexeme().size(),
                     ";"
                     "expected \';\' at end of declaration"));
    } else {
      return tl::unexpected(ClangError(
          ClangErrorSeverity::ERROR, lexer.getFileName(), sc.getLineNo(),
          sc.getColumnNo(), ";", "expected \';\' at end of declaration"));
    }
  }

  auto type = (optDims.has_value())
                  ? ctx.getArrayType(numType->second, optDims.value())
                  : numType->second;
  auto innerSourceLoc =
      SourceLocation(numType->first.getLineNo(), numType->first.getColumnNo(),
                     sc.getLineNo(), sc.getColumnNo(), lexer.getFileName());
  auto inner =
      util::allocateNode<VarDecl>(ctx, ident, type, nullptr, innerSourceLoc);

  if (!inner) {
    return tl::unexpected(inner.error());
  }

  return util::allocateNode<DeclStmt>(ctx, inner.value(),
                                      std::move(innerSourceLoc));
}

// Assert correct first token.
//
// stmt_list ::= stmt+
auto Parser::parseStmtList(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<std::vector<ASTNode *>, ClangError> {
  auto stmtList = std::vector<ASTNode *>();
  auto currToken = this->peekNextToken(lexer);
  while (currToken.in(util::FIRST_stmt)) {
    auto stmt = parseStmt(lexer, ctx);
    if (!stmt) {
      return tl::unexpected(stmt.error());
    }

    if (stmt.value() != nullptr)
      stmtList.push_back(stmt.value());
    currToken = this->peekNextToken(lexer);
  }

  if (stmtList.empty()) {
    return util::badParseCase();
  }

  return stmtList;
}

// stmt ::= expr_stmt | block | if_stmt | while_stmt | return_stmt
auto Parser::parseStmt(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<ASTNode *, ClangError> {
  auto firstToken = this->peekNextToken(lexer);
  if (firstToken.in(util::FIRST_expr_stmt)) {
    return parseExprStmt(lexer, ctx);
  } else if (firstToken.in(util::FIRST_block)) {
    return parseBlock(lexer, ctx);
  } else if (firstToken.in(util::FIRST_if_stmt)) {
    return parseIfStmt(lexer, ctx);
  } else if (firstToken.in(util::FIRST_while_stmt)) {
    return parseWhileStmt(lexer, ctx);
  } else if (firstToken.in(util::FIRST_return_stmt)) {
    return parseReturnStmt(lexer, ctx);
  } else {
    return util::badParseCase();
  }
}

// Verify first token is correct before use.
//
// expr_stmt ::= [expr] ";"
auto Parser::parseExprStmt(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<Expr *, ClangError> {
  auto firstToken = this->peekNextToken(lexer);
  if (firstToken.getTokenType() == TokenType::SC) {
    this->getNextToken(lexer);
    return nullptr;
  } else if (firstToken.in(util::FIRST_expr)) {
    auto expr = parseExpr(lexer, ctx);
    if (!expr) {
      return tl::unexpected(expr.error());
    }

    auto sc = this->getNextToken(lexer);
    if (sc.getTokenType() != TokenType::SC) {
      std::cout << sc << std::endl;
      this->getNextToken(lexer);
      return tl::unexpected(
          ClangError(ClangErrorSeverity::ERROR, lexer.getFileName(),
                     this->lastTokenConsumed->getLineNo(),
                     this->lastTokenConsumed->getColumnNo() +
                         this->lastTokenConsumed->getLexeme().size(),
                     "expected \';\' after expression"));
    }

    return expr.value();
  } else {
    return util::badParseCase();
  }
}

// while_stmt ::= "while" "(" expr ")" stmt
auto Parser::parseWhileStmt(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<WhileStmt *, ClangError> {
  auto whileTok = this->getNextToken(lexer);
  if (whileTok.getTokenType() != TokenType::WHILE) {
    return util::badParseCase();
  }

  auto lpar = this->getNextToken(lexer);
  if (lpar.getTokenType() != TokenType::LPAR) {
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(), whileTok.getLineNo(),
        whileTok.getColumnNo() + whileTok.getLexeme().size(),
        "expected \'(\' after \'while\'"));
  }

  auto lookahead = this->peekNextToken(lexer);
  if (!lookahead.in(util::FIRST_expr)) {
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(), lpar.getLineNo(),
        lpar.getColumnNo() + lpar.getLexeme().size(), "expected expression"));
  }

  auto expr = parseExpr(lexer, ctx);
  if (!expr) {
    return tl::unexpected(expr.error());
  }

  auto rpar = this->getNextToken(lexer);
  if (rpar.getTokenType() != TokenType::RPAR) {
    this->getNextToken(lexer);
    return tl::unexpected(
        buildBraceError(lexer.getFileName(), lpar, rpar, '(', ')'));
  }

  lookahead = this->peekNextToken(lexer);
  if (!lookahead.in(util::FIRST_stmt)) {
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(), rpar.getLineNo(),
        rpar.getColumnNo() + 1, "expected expression or statement"));
  }

  auto stmt = parseStmt(lexer, ctx);
  if (!stmt) {
    return tl::unexpected(stmt.error());
  }

  return util::allocateNode<WhileStmt>(
      ctx, expr.value(), stmt.value(),
      SourceLocation(whileTok.getLineNo(), whileTok.getColumnNo(),
                     stmt.value()->getLocation().endLineNo,
                     stmt.value()->getLocation().endColumnNo,
                     lexer.getFileName()));
}

// if_stmt ::= "if" "(" expr ")" block [else_stmt]
auto Parser::parseIfStmt(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<IfStmt *, ClangError> {
  auto ifTok = this->getNextToken(lexer);
  if (!ifTok.in(util::FIRST_if_stmt)) {
    return util::badParseCase();
  }

  auto lpar = this->getNextToken(lexer);
  if (lpar.getTokenType() != TokenType::LPAR) {
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(), ifTok.getLineNo(),
        ifTok.getColumnNo() + ifTok.getLexeme().size(),
        "expected \'(\' after \'if\'"));
  }

  auto lookahead = this->peekNextToken(lexer);
  if (!lookahead.in(util::FIRST_expr)) {
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(), lpar.getLineNo(),
        lpar.getColumnNo() + lpar.getLexeme().size(), "expected expression"));
  }

  auto expr = parseExpr(lexer, ctx);
  if (!expr) {
    return tl::unexpected(expr.error());
  }

  auto rpar = this->getNextToken(lexer);
  if (rpar.getTokenType() != TokenType::RPAR) {
    this->getNextToken(lexer);
    return tl::unexpected(
        buildBraceError(lexer.getFileName(), lpar, rpar, '(', ')'));
  }

  lookahead = this->peekNextToken(lexer);
  if (!lookahead.in(util::FIRST_block)) {
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(), rpar.getLineNo(),
        rpar.getColumnNo() + 1, "expected \'{\' after condition"));
  }

  auto block = parseBlock(lexer, ctx);
  if (!block) {
    return tl::unexpected(block.error());
  }

  lookahead = this->peekNextToken(lexer);
  ASTNode *elseStmt = nullptr;
  if (lookahead.in(util::FIRST_else_stmt)) {
    auto parsedElseStmt = parseElseStmt(lexer, ctx);
    if (!parsedElseStmt) {
      return tl::unexpected(parsedElseStmt.error());
    }

    elseStmt = parsedElseStmt.value();
  }

  return util::allocateNode<IfStmt>(
      ctx, expr.value(), block.value(), elseStmt,
      SourceLocation(
          ifTok.getLineNo(), ifTok.getColumnNo(),
          (elseStmt != nullptr) ? elseStmt->getLocation().endLineNo
                                : block.value()->getLocation().endLineNo,
          (elseStmt != nullptr) ? elseStmt->getLocation().endColumnNo
                                : block.value()->getLocation().endColumnNo,
          lexer.getFileName()));
}

// else_stmt ::= "else" block
auto Parser::parseElseStmt(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<CompoundStmt *, ClangError> {
  auto elseTok = this->getNextToken(lexer);
  if (elseTok.getTokenType() != TokenType::ELSE) {
    return util::badParseCase();
  }

  auto lookahead = this->peekNextToken(lexer, 1);
  if (!lookahead.in(util::FIRST_block)) {
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(), elseTok.getLineNo(),
        elseTok.getColumnNo() + elseTok.getLexeme().size(),
        "expected \'{\' after \'else\'"));
  }

  return parseBlock(lexer, ctx);
}

// return_stmt ::= "return" ";" | "return" expr ";"
auto Parser::parseReturnStmt(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<ReturnStmt *, ClangError> {
  auto returnTok = this->getNextToken(lexer);
  if (returnTok.getTokenType() != TokenType::RETURN) {
    return util::badParseCase();
  }

  auto lookahead = this->peekNextToken(lexer);
  if (lookahead.getTokenType() == TokenType::SC) {
    this->getNextToken(lexer);
    return util::allocateNode<ReturnStmt>(
        ctx, nullptr,
        SourceLocation(returnTok.getLineNo(), returnTok.getColumnNo(),
                       lookahead.getLineNo(), lookahead.getColumnNo(),
                       lexer.getFileName()));
  } else if (lookahead.in(util::FIRST_expr)) {
    auto expr = parseExpr(lexer, ctx);
    if (!expr) {
      return tl::unexpected(expr.error());
    }

    auto sc = this->getNextToken(lexer);
    if (sc.getTokenType() == TokenType::SC) {
      return util::allocateNode<ReturnStmt>(
          ctx, expr.value(),
          SourceLocation(returnTok.getLineNo(), returnTok.getColumnNo(),
                         sc.getLineNo(), sc.getColumnNo(),
                         lexer.getFileName()));
    }
  }

  this->getNextToken(lexer);
  return tl::unexpected(ClangError(
      ClangErrorSeverity::ERROR, lexer.getFileName(),
      lastTokenConsumed->getLineNo(),
      lastTokenConsumed->getColumnNo() + lastTokenConsumed->getLexeme().size(),
      ";", "expected \';\' after return statement"));
}

// Check first is valid
//
// lvalue ::= IDENT [dimensions]
auto Parser::parseLValue(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<Expr *, ClangError> {
  auto ident = this->getNextToken(lexer);
  if (ident.getTokenType() != TokenType::IDENT) {
    return util::badParseCase();
  }

  auto lookahead = this->peekNextToken(lexer);
  auto optDims = std::optional<std::vector<Expr *>>();
  if (lookahead.getTokenType() == TokenType::LBOX) {
    auto dims = parseDimensions<false>(lexer, ctx);
    if (!dims) {
      return tl::unexpected(dims.error());
    }

    optDims = dims.value();
  }

  auto identSource = SourceLocation(
      ident.getLineNo(), ident.getColumnNo(), ident.getLineNo(),
      ident.getColumnNo() + ident.getLexeme().size(), lexer.getFileName());
  auto declRefExpr = util::allocateNode<DeclRefExpr>(ctx, ident, identSource);
  if (!declRefExpr)
    return tl::unexpected(declRefExpr.error());

  auto rootExpr = (Expr *)declRefExpr.value();
  if (!optDims.has_value()) {
    return rootExpr;
  }

  for (const auto dimExpr : optDims.value()) {
    auto implicitCast =
        util::allocateNode<ImplicitCastExpr>(ctx, rootExpr, identSource);
    if (!implicitCast)
      return tl::unexpected(implicitCast.error());

    auto arraySubscriptExpr = util::allocateNode<ArraySubscriptExpr>(
        ctx, implicitCast.value(), dimExpr,
        SourceLocation(ident.getLineNo(), ident.getColumnNo(),
                       lastTokenConsumed->getLineNo(),
                       lastTokenConsumed->getColumnNo(), lexer.getFileName()));
    if (!arraySubscriptExpr)
      return tl::unexpected(arraySubscriptExpr.error());

    rootExpr = arraySubscriptExpr.value();
  }

  return rootExpr;
}

auto buildBinaryExprTree(std::vector<Expr *> arguments,
                         std::vector<Token> operators, ASTContext &ctx)
    -> tl::expected<Expr *, ClangError> {
  assert(operators.size() + 1 == arguments.size());

  const auto n = static_cast<int>(operators.size());
  auto root = arguments.front();
  for (auto i = 0; i < n; i++) {
    auto binaryExpr = util::allocateNode<BinaryOperator>(
        ctx, operators[i], root, arguments[i + 1],
        SourceLocation(root->getLocation().startLineNo,
                       root->getLocation().startColumnNo,
                       arguments[i + 1]->getLocation().endLineNo,
                       arguments[i + 1]->getLocation().endColumnNo,
                       root->getLocation().fileName.value()));
    if (!binaryExpr) {
      return tl::unexpected(binaryExpr.error());
    }

    root = binaryExpr.value();
  }

  return root;
}

// expr ::= or_expr ("=" or_expr)*
auto Parser::parseExpr(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<Expr *, ClangError> {
  auto firstToken = this->peekNextToken(lexer);
  if (!firstToken.in(util::FIRST_or_expr)) {
    return util::badParseCase();
  }

  static constexpr auto validTokens =
      std::array<TokenType, 1>{TokenType::ASSIGN};
  auto processed = buildBinaryExprVectors(
      *this, lexer, ctx, validTokens,
      [](Parser &parser, Lexer &lexer,
         ASTContext &ctx) -> tl::expected<Expr *, ClangError> {
        return parser.parseOrExpr(lexer, ctx);
      });

  if (!processed) {
    return tl::unexpected(processed.error());
  }

  auto &&[exprs, toks] = std::move(processed.value());
  return buildBinaryExprTree(std::move(exprs), std::move(toks), ctx);
}

// or_expr ::= and_expr ("||" and_expr)*
auto Parser::parseOrExpr(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<Expr *, ClangError> {
  auto firstToken = this->peekNextToken(lexer);
  if (!firstToken.in(util::FIRST_and_expr)) {
    return util::badParseCase();
  }

  static constexpr auto validTokens = std::array<TokenType, 1>{TokenType::OR};
  auto processed = buildBinaryExprVectors(
      *this, lexer, ctx, validTokens,
      [](Parser &parser, Lexer &lexer,
         ASTContext &ctx) -> tl::expected<Expr *, ClangError> {
        return parser.parseAndExpr(lexer, ctx);
      });

  if (!processed) {
    return tl::unexpected(processed.error());
  }

  auto &&[exprs, toks] = std::move(processed.value());
  return buildBinaryExprTree(std::move(exprs), std::move(toks), ctx);
}

// and_expr ::= eq_expr ("&&" eq_expr)*
auto Parser::parseAndExpr(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<Expr *, ClangError> {
  auto firstToken = this->peekNextToken(lexer);
  if (!firstToken.in(util::FIRST_eq_expr)) {
    return util::badParseCase();
  }

  static constexpr auto validTokens = std::array<TokenType, 1>{TokenType::AND};
  auto processed = buildBinaryExprVectors(
      *this, lexer, ctx, validTokens,
      [](Parser &parser, Lexer &lexer,
         ASTContext &ctx) -> tl::expected<Expr *, ClangError> {
        return parser.parseEqExpr(lexer, ctx);
      });

  if (!processed) {
    return tl::unexpected(processed.error());
  }

  auto &&[exprs, toks] = std::move(processed.value());
  return buildBinaryExprTree(std::move(exprs), std::move(toks), ctx);
}

// eq_expr ::= rel_expr (("==" | "!=") rel_expr)*
auto Parser::parseEqExpr(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<Expr *, ClangError> {
  auto firstToken = this->peekNextToken(lexer);
  if (!firstToken.in(util::FIRST_rel_expr)) {
    return util::badParseCase();
  }

  static constexpr auto validTokens =
      std::array<TokenType, 2>{TokenType::EQ, TokenType::NE};
  auto processed = buildBinaryExprVectors(
      *this, lexer, ctx, validTokens,
      [](Parser &parser, Lexer &lexer,
         ASTContext &ctx) -> tl::expected<Expr *, ClangError> {
        return parser.parseRelExpr(lexer, ctx);
      });

  if (!processed) {
    return tl::unexpected(processed.error());
  }

  auto &&[exprs, toks] = std::move(processed.value());
  return buildBinaryExprTree(std::move(exprs), std::move(toks), ctx);
}

// rel_expr ::= add_expr (("<=" | "<" | ">=" | ">") add_expr)*
auto Parser::parseRelExpr(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<Expr *, ClangError> {
  auto firstToken = this->peekNextToken(lexer);
  if (!firstToken.in(util::FIRST_add_expr)) {
    return util::badParseCase();
  }

  static constexpr auto validTokens = std::array<TokenType, 4>{
      TokenType::LE, TokenType::LT, TokenType::GE, TokenType::GT};
  auto processed = buildBinaryExprVectors(
      *this, lexer, ctx, validTokens,
      [](Parser &parser, Lexer &lexer,
         ASTContext &ctx) -> tl::expected<Expr *, ClangError> {
        return parser.parseAddExpr(lexer, ctx);
      });

  if (!processed) {
    return tl::unexpected(processed.error());
  }

  auto &&[exprs, toks] = std::move(processed.value());
  return buildBinaryExprTree(std::move(exprs), std::move(toks), ctx);
}

// add_expr ::= mul_expr (("+" | "-") mul_expr)*
auto Parser::parseAddExpr(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<Expr *, ClangError> {
  auto firstToken = this->peekNextToken(lexer);
  if (!firstToken.in(util::FIRST_mul_expr)) {
    return util::badParseCase();
  }

  static constexpr auto validTokens =
      std::array<TokenType, 2>{TokenType::PLUS, TokenType::MINUS};
  auto processed = buildBinaryExprVectors(
      *this, lexer, ctx, validTokens,
      [](Parser &parser, Lexer &lexer,
         ASTContext &ctx) -> tl::expected<Expr *, ClangError> {
        return parser.parseMulExpr(lexer, ctx);
      });

  if (!processed) {
    return tl::unexpected(processed.error());
  }

  auto &&[exprs, toks] = std::move(processed.value());
  return buildBinaryExprTree(std::move(exprs), std::move(toks), ctx);
}

// mul_expr ::= unary_expr (("*" | "/" | "%") unary_expr)*
auto Parser::parseMulExpr(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<Expr *, ClangError> {
  auto firstToken = this->peekNextToken(lexer);
  if (!firstToken.in(util::FIRST_unary_expr)) {
    return util::badParseCase();
  }

  static constexpr auto validTokens = std::array<TokenType, 3>{
      TokenType::ASTERIX, TokenType::DIV, TokenType::MOD};
  auto processed = buildBinaryExprVectors(
      *this, lexer, ctx, validTokens,
      [](Parser &parser, Lexer &lexer,
         ASTContext &ctx) -> tl::expected<Expr *, ClangError> {
        return parser.parseUnaryExpr(lexer, ctx);
      });

  if (!processed) {
    return tl::unexpected(processed.error());
  }

  auto &&[exprs, toks] = std::move(processed.value());
  return buildBinaryExprTree(std::move(exprs), std::move(toks), ctx);
}

// unary_expr ::= "-" unary_expr | "!" unary_expr | primary
auto Parser::parseUnaryExpr(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<Expr *, ClangError> {
  auto firstToken = this->peekNextToken(lexer);
  if (firstToken.getTokenType() == TokenType::MINUS ||
      firstToken.getTokenType() == TokenType::NOT) {
    this->getNextToken(lexer);
    auto nextUnary = parseUnaryExpr(lexer, ctx);
    if (!nextUnary) {
      return tl::unexpected(nextUnary.error());
    }

    return util::allocateNode<UnaryOperator>(
        ctx, firstToken, nextUnary.value(),
        SourceLocation(firstToken.getLineNo(), firstToken.getColumnNo(),
                       nextUnary.value()->getLocation().endLineNo,
                       nextUnary.value()->getLocation().endColumnNo,
                       lexer.getFileName()));
  } else if (firstToken.in(util::FIRST_primary)) {
    auto primary = parsePrimary(lexer, ctx);
    if (!primary) {
      return tl::unexpected(primary.error());
    }

    return primary.value();
  } else {
    return util::badParseCase();
  }
}

// primary ::= "(" expr ")" | IDENT "(" args ")" | lvalue | INT_LIT | FLOAT_LIT
// | BOOL_LIT
auto Parser::parsePrimary(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<Expr *, ClangError> {
  auto firstToken = this->peekNextToken(lexer);
  auto secondToken = this->peekNextToken(lexer, 2);
  if (firstToken.getTokenType() == TokenType::LPAR) {
    auto lpar = this->getNextToken(lexer);
    auto expr = parseExpr(lexer, ctx);
    if (!expr) {
      return tl::unexpected(expr.error());
    }

    auto rpar = this->getNextToken(lexer);
    if (rpar.getTokenType() != TokenType::RPAR) {
      this->getNextToken(lexer);
      return tl::unexpected(buildBraceError(lexer.getFileName(), lpar,
                                            *lastTokenConsumed, '(', ')'));
    }
    return util::allocateNode<ParenExpr>(
        ctx, expr.value(), SourceLocation(lpar, rpar, lexer.getFileName()));
  } else if (firstToken.getTokenType() == TokenType::IDENT &&
             secondToken.getTokenType() == TokenType::LPAR) {
    auto ident = this->getNextToken(lexer);
    assert(ident.getTokenType() == TokenType::IDENT);
    auto lpar = this->getNextToken(lexer);
    assert(lpar.getTokenType() == TokenType::LPAR);
    auto args = parseArgs(lexer, ctx);
    if (!args) {
      return tl::unexpected(args.error());
    }

    auto rpar = this->getNextToken(lexer);
    if (rpar.getTokenType() != TokenType::RPAR) {
      this->getNextToken(lexer);
      return tl::unexpected(buildBraceError(lexer.getFileName(), lpar,
                                            *lastTokenConsumed, '(', ')'));
    }

    auto ref = util::allocateNode<DeclRefExpr>(
        ctx, ident, SourceLocation(ident, lexer.getFileName()));
    if (!ref) {
      return tl::unexpected(ref.error());
    }

    return util::allocateNode<CallExpr>(
        ctx, ref.value(), std::move(args.value()),
        SourceLocation(ident.getLineNo(), ident.getColumnNo(), rpar.getLineNo(),
                       rpar.getColumnNo(), lexer.getFileName()));
  } else if (firstToken.getTokenType() == TokenType::IDENT) {
    auto lvalue = parseLValue(lexer, ctx);
    if (!lvalue) {
      return tl::unexpected(lvalue.error());
    }

    return lvalue.value();
  } else if (firstToken.getTokenType() == TokenType::INT_LIT) {
    this->getNextToken(lexer);
    return util::allocateNode<IntegerLiteral>(
        ctx, firstToken.asInt(),
        SourceLocation(firstToken, lexer.getFileName()));
  } else if (firstToken.getTokenType() == TokenType::FLOAT_LIT) {
    this->getNextToken(lexer);
    return util::allocateNode<FloatLiteral>(
        ctx, firstToken.asFloat(),
        SourceLocation(firstToken, lexer.getFileName()));
  } else if (firstToken.getTokenType() == TokenType::BOOL_LIT) {
    this->getNextToken(lexer);
    return util::allocateNode<BoolLiteral>(
        ctx, firstToken.asBool(),
        SourceLocation(firstToken, lexer.getFileName()));
  } else {
    return util::badParseCase();
  }
}

// args ::= [arg_list]
auto Parser::parseArgs(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<std::vector<Expr *>, ClangError> {
  auto lookahead = this->peekNextToken(lexer);
  std::cout << "with lookahead " << lookahead << std::endl;
  if (lookahead.in(util::FOLLOW_args)) {
    return {};
  }

  if (lookahead.in(util::FIRST_arg_list)) {
    auto argList = parseArgList(lexer, ctx);
    if (!argList) {
      return tl::unexpected(argList.error());
    }

    return argList.value();
  } else {
    this->getNextToken(lexer);
    return tl::unexpected(ClangError(
        ClangErrorSeverity::ERROR, lexer.getFileName(),
        lastTokenConsumed->getLineNo(), lastTokenConsumed->getColumnNo() + 1,
        "expected expression"));
  }
}

// arg_list ::= expr ("," expr)*
auto Parser::parseArgList(Lexer &lexer, ASTContext &ctx)
    -> tl::expected<std::vector<Expr *>, ClangError> {
  auto firstToken = this->peekNextToken(lexer);
  if (!firstToken.in(util::FIRST_expr)) {
    return util::badParseCase();
  }

  static constexpr auto validTokens =
      std::array<TokenType, 1>{TokenType::COMMA};
  auto firstExpr = parseExpr(lexer, ctx);
  if (!firstExpr) {
    return tl::unexpected(firstExpr.error());
  }

  auto exprs = std::vector<Expr *>{firstExpr.value()};
  auto lookahead = this->peekNextToken(lexer);
  while (lookahead.in(validTokens)) {
    this->getNextToken(lexer);
    auto expr = parseExpr(lexer, ctx);
    if (!expr) {
      return tl::unexpected(expr.error());
    }

    exprs.push_back(expr.value());
    lookahead = this->peekNextToken(lexer);
  }

  return exprs;
}

} // namespace mccomp
