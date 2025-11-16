#include <ast/ast.h>
#include <deque>
#include <lexer/lexer.h>
#include <tokens/tokens.h>

namespace mccomp {

/// LogError* - These are little helper function for error handling.
std::unique_ptr<ASTnode> LogError(Token tok, const char *Str) {
  fprintf(stderr, "%d:%d Error: %s\n", tok.getLineNo(), tok.getColumnNo(), Str);
  exit(2);
  return nullptr;
}

std::unique_ptr<FunctionPrototypeAST> LogErrorP(Token tok, const char *Str) {
  LogError(tok, Str);
  exit(2);
  return nullptr;
}

std::unique_ptr<ASTnode> LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  exit(2);
  return nullptr;
}

//===----------------------------------------------------------------------===//
// Recursive Descent - Function call for each production
//===----------------------------------------------------------------------===//

class Parser {
  /// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the
  /// current token the parser is looking at.  getNextToken reads another token
  /// from the lexer and updates CurTok with its results.
  std::optional<Token> currentToken;
  std::deque<Token> tokenBuffer;

public:
  auto getNextToken(Lexer &lexer) -> Token {
    if (tokenBuffer.size() == 0)
      tokenBuffer.push_back(lexer.getToken());

    auto temp = tokenBuffer.front();
    tokenBuffer.pop_front();
    currentToken = temp;

    return temp;
  }

  auto peekNextToken(const Lexer &lexer) const -> Token {
    if (tokenBuffer.size() == 0)
      return lexer.peekToken();

    auto token =
        (tokenBuffer.size() == 0) ? lexer.peekToken() : tokenBuffer.front();

    return token;
  }

  // element ::= FLOAT_LIT
  std::unique_ptr<ASTnode> ParseFloatNumberExpr(Lexer &lexer) {
    auto curr = curTok.value();
    auto Result = std::make_unique<FloatASTnode>(curr, curr.getFloat().value());
    getNextToken(lexer); // consume the number
    return std::move(Result);
  }

  // element ::= INT_LIT
  std::unique_ptr<ASTnode> ParseIntNumberExpr(Lexer &lexer) {
    auto Result = std::make_unique<IntASTnode>(curTok.value(),
                                               curTok.value().getInt().value());
    getNextToken(lexer); // consume the number
    return std::move(Result);
  }

  // element ::= BOOL_LIT
  std::unique_ptr<ASTnode> ParseBoolExpr(Lexer &lexer) {
    auto Result = std::make_unique<BoolASTnode>(
        curTok.value(), curTok.value().getBool().value());
    getNextToken(lexer); // consume the number
    return std::move(Result);
  }

  // param_list_prime ::= "," param param_list_prime
  //                   |  ε
  std::vector<std::unique_ptr<ParamAST>> ParseParamListPrime(Lexer &lexer) {
    std::vector<std::unique_ptr<ParamAST>> param_list;
    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();

    if (currTokType == TokenType::COMMA) { // more parameters in list
      getNextToken(lexer);                 // eat ","

      auto param = ParseParam(lexer);
      if (param) {
        printf("found param in param_list_prime: %s\n",
               param->getName().c_str());
        param_list.push_back(std::move(param));
        auto param_list_prime = ParseParamListPrime(lexer);
        for (unsigned i = 0; i < param_list_prime.size(); i++) {
          param_list.push_back(std::move(param_list_prime.at(i)));
        }
      }
    } else if (curr.getTokenType() ==
               TokenType::RPAR) { // FOLLOW(param_list_prime)
      // expand by param_list_prime ::= ε
      // do nothing
    } else {
      LogError(curr, "expected ',' or ')' in list of parameter declarations");
    }

    return param_list;
  }

  // param ::= var_type IDENT
  std::unique_ptr<ParamAST> ParseParam(Lexer &lexer) {
    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();

    std::string type =
        curr.getLexeme().value(); // keep track of the type of the param
    getNextToken(lexer);          // eat the type token
    std::unique_ptr<ParamAST> P;

    if (currTokType == TokenType::IDENT) { // parameter declaration
      std::string Name = curr.getIdent().value();
      getNextToken(lexer); // eat "IDENT"
    }

    return P;
  }

  // param_list ::= param param_list_prime
  std::vector<std::unique_ptr<ParamAST>> ParseParamList(Lexer &lexer) {
    std::vector<std::unique_ptr<ParamAST>> param_list;

    auto param = ParseParam(lexer);
    if (param) {
      param_list.push_back(std::move(param));
      auto param_list_prime = ParseParamListPrime(lexer);
      for (unsigned i = 0; i < param_list_prime.size(); i++) {
        param_list.push_back(std::move(param_list_prime.at(i)));
      }
    }

    return param_list;
  }

  // params ::= param_list
  //         |  ε
  std::vector<std::unique_ptr<ParamAST>> ParseParams(Lexer &lexer) {
    std::vector<std::unique_ptr<ParamAST>> param_list;

    std::string Type;
    std::string Name = "";

    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();
    if (currTokType == TokenType::INT_TOK ||
        currTokType == TokenType::FLOAT_TOK ||
        currTokType == TokenType::BOOL_TOK) { // FIRST(param_list)

      auto list = ParseParamList(lexer);
      for (unsigned i = 0; i < list.size(); i++) {
        param_list.push_back(std::move(list.at(i)));
      }

    } else if (currTokType == TokenType::VOID_TOK) { // FIRST("void")
      // void
      // check that the next token is a )
      getNextToken(lexer); // eat 'void'
      if (currTokType != TokenType::RPAR) {
        LogError(curr, "expected ')', after 'void' in \
       end of function declaration");
      }
    } else if (currTokType == TokenType::RPAR) { // FOLLOW(params)
      // expand by params ::= ε
      // do nothing
    } else {
      LogError(
          curr,
          "expected 'int', 'bool' or 'float' in function declaration or ') in \
       end of function declaration");
    }

    return param_list;
  }

  /*** TODO : Task 2 - Parser ***

  // args ::= arg_list
  //      |  ε
  // arg_list ::= arg_list "," expr
  //      | expr

  // rval ::= rval "||" rval
  //      | rval "&&" rval
  //      | rval "==" rval | rval "!=" rval
  //      | rval "<=" rval | rval "<" rval | rval ">=" rval | rval ">" rval
  //      | rval "+" rval | rval "-" rval
  //      | rval "*" rval | rval "/" rval | rval "%" rval
  //      | "-" rval | "!" rval
  //      | "(" expr ")"
  //      | IDENT | IDENT "(" args ")"
  //      | INT_LIT | FLOAT_LIT | BOOL_LIT
  **/

  // expr ::= IDENT "=" expr
  //      |  rval
  std::unique_ptr<ASTnode> ParseExper(Lexer &lexer) {
    //
    // TO BE COMPLETED
    //
    return nullptr;
  }

  // expr_stmt ::= expr ";"
  //            |  ";"
  std::unique_ptr<ASTnode> ParseExperStmt(Lexer &lexer) {
    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();
    if (currTokType == TokenType::SC) { // empty statement
      getNextToken(lexer);              // eat ;
      return nullptr;
    } else {
      auto expr = ParseExper(lexer);
      if (expr) {
        if (currTokType == TokenType::SC) {
          getNextToken(lexer); // eat ;
          return expr;
        } else {
          LogError(curr, "expected ';' to end expression statement");
        }
      } else
        return nullptr;
    }
    return nullptr;
  }

  // else_stmt  ::= "else" block
  //             |  ε
  std::unique_ptr<ASTnode> ParseElseStmt(Lexer &lexer) {
    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();
    if (currTokType == TokenType::ELSE) { // FIRST(else_stmt)
      // expand by else_stmt  ::= "else" "{" stmt "}"
      getNextToken(lexer); // eat "else"

      if (!(currTokType == TokenType::LBRA)) {
        return LogError(
            curr, "expected { to start else block of if-then-else statment");
      }
      auto Else = ParseBlock(lexer);
      if (!Else)
        return nullptr;
      return Else;
    } else if (currTokType == TokenType::NOT ||
               currTokType == TokenType::MINUS ||
               currTokType == TokenType::PLUS ||
               currTokType == TokenType::LPAR ||
               currTokType == TokenType::IDENT ||
               currTokType == TokenType::INT_LIT ||
               currTokType == TokenType::BOOL_LIT ||
               currTokType == TokenType::FLOAT_LIT ||
               currTokType == TokenType::SC || currTokType == TokenType::LBRA ||
               currTokType == TokenType::WHILE ||
               currTokType == TokenType::IF || currTokType == TokenType::ELSE ||
               currTokType == TokenType::RETURN ||
               currTokType == TokenType::RBRA) { // FOLLOW(else_stmt)
      // expand by else_stmt  ::= ε
      // return an empty statement
      return nullptr;
    } else
      LogError(curr, "expected 'else' or one of \
    '!', '-', '+', '(' , IDENT , INT_LIT, BOOL_LIT, FLOAT_LIT, ';', \
    '{', 'while', 'if', 'else', ε, 'return', '}' ");

    return nullptr;
  }

  // if_stmt ::= "if" "(" expr ")" block else_stmt
  std::unique_ptr<ASTnode> ParseIfStmt(Lexer &lexer) {
    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();

    getNextToken(lexer); // eat the if.
    if (currTokType == TokenType::LPAR) {
      getNextToken(lexer); // eat (
      // condition.
      auto Cond = ParseExper(lexer);
      if (!Cond)
        return nullptr;
      if (currTokType != TokenType::RPAR)
        return LogError(curr, "expected )");
      getNextToken(lexer); // eat )

      if (!(currTokType == TokenType::LBRA)) {
        return LogError(curr, "expected { to start then block of if statment");
      }

      auto Then = ParseBlock(lexer);
      if (!Then)
        return nullptr;
      auto Else = ParseElseStmt(lexer);

      return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
                                         std::move(Else));

    } else
      return LogError(curr, "expected (");

    return nullptr;
  }

  // return_stmt ::= "return" ";"
  //             |  "return" expr ";"
  std::unique_ptr<ASTnode> ParseReturnStmt(Lexer &lexer) {
    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();

    getNextToken(lexer); // eat the return
    if (currTokType == TokenType::SC) {
      getNextToken(lexer); // eat the ;
      // return a null value
      return std::make_unique<ReturnAST>(std::move(nullptr));
    } else if (currTokType == TokenType::NOT ||
               currTokType == TokenType::MINUS ||
               currTokType == TokenType::PLUS ||
               currTokType == TokenType::LPAR ||
               currTokType == TokenType::IDENT ||
               currTokType == TokenType::BOOL_LIT ||
               currTokType == TokenType::INT_LIT ||
               currTokType == TokenType::FLOAT_LIT) { // FIRST(expr)
      auto val = ParseExper(lexer);
      if (!val)
        return nullptr;

      if (currTokType == TokenType::SC) {
        getNextToken(lexer); // eat the ;
        return std::make_unique<ReturnAST>(std::move(val));
      } else
        return LogError(curr, "expected ';'");
    } else
      return LogError(curr, "expected ';' or expression");

    return nullptr;
  }

  // while_stmt ::= "while" "(" expr ")" stmt
  std::unique_ptr<ASTnode> ParseWhileStmt(Lexer &lexer) {
    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();

    getNextToken(lexer); // eat the while.
    if (currTokType == TokenType::LPAR) {
      getNextToken(lexer); // eat (
      // condition.
      auto Cond = ParseExper(lexer);
      if (!Cond)
        return nullptr;
      if (currTokType != TokenType::RPAR)
        return LogError(curr, "expected )");
      getNextToken(lexer); // eat )

      auto Body = ParseStmt(lexer);
      if (!Body)
        return nullptr;

      return std::make_unique<WhileExprAST>(std::move(Cond), std::move(Body));
    } else
      return LogError(curr, "expected (");
  }

  // stmt ::= expr_stmt
  //      |  block
  //      |  if_stmt
  //      |  while_stmt
  //      |  return_stmt
  std::unique_ptr<ASTnode> ParseStmt(Lexer &lexer) {
    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();
    if (currTokType == TokenType::NOT || currTokType == TokenType::MINUS ||
        currTokType == TokenType::PLUS || currTokType == TokenType::LPAR ||
        currTokType == TokenType::IDENT || currTokType == TokenType::BOOL_LIT ||
        currTokType == TokenType::INT_LIT ||
        currTokType == TokenType::FLOAT_LIT ||
        currTokType == TokenType::SC) { // FIRST(expr_stmt)
      // expand by stmt ::= expr_stmt
      auto expr_stmt = ParseExperStmt(lexer);
      fprintf(stderr, "Parsed an expression statement\n");
      return expr_stmt;
    } else if (currTokType == TokenType::LBRA) { // FIRST(block)
      auto block_stmt = ParseBlock(lexer);
      if (block_stmt) {
        fprintf(stderr, "Parsed a block\n");
        return block_stmt;
      }
    } else if (currTokType == TokenType::IF) { // FIRST(if_stmt)
      auto if_stmt = ParseIfStmt(lexer);
      if (if_stmt) {
        fprintf(stderr, "Parsed an if statment\n");
        return if_stmt;
      }
    } else if (currTokType == TokenType::WHILE) { // FIRST(while_stmt)
      auto while_stmt = ParseWhileStmt(lexer);
      if (while_stmt) {
        fprintf(stderr, "Parsed a while statment\n");
        return while_stmt;
      }
    } else if (currTokType == TokenType::RETURN) { // FIRST(return_stmt)
      auto return_stmt = ParseReturnStmt(lexer);
      if (return_stmt) {
        fprintf(stderr, "Parsed a return statment\n");
        return return_stmt;
      }
    }
    // else if(currTokType == RBRA) { // FOLLOW(stmt_list_prime)
    //  expand by stmt_list_prime ::= ε
    //  do nothing
    //}
    else { // syntax error
      return LogError(curr, "expected BLA BLA\n");
    }
    return nullptr;
  }

  // stmt_list ::= stmt stmt_list_prime
  std::vector<std::unique_ptr<ASTnode>> ParseStmtList(Lexer &lexer) {
    std::vector<std::unique_ptr<ASTnode>> stmt_list; // vector of statements
    auto stmt = ParseStmt(lexer);
    if (stmt) {
      stmt_list.push_back(std::move(stmt));
    }
    auto stmt_list_prime = ParseStmtListPrime(lexer);
    for (unsigned i = 0; i < stmt_list_prime.size(); i++) {
      stmt_list.push_back(std::move(stmt_list_prime.at(i)));
    }
    return stmt_list;
  }

  // stmt_list_prime ::= stmt stmt_list_prime
  //                  |  ε
  std::vector<std::unique_ptr<ASTnode>> ParseStmtListPrime(Lexer &lexer) {
    std::vector<std::unique_ptr<ASTnode>> stmt_list; // vector of statements
    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();

    if (currTokType == TokenType::NOT || currTokType == TokenType::MINUS ||
        currTokType == TokenType::PLUS || currTokType == TokenType::LPAR ||
        currTokType == TokenType::IDENT || currTokType == TokenType::BOOL_LIT ||
        currTokType == TokenType::INT_LIT ||
        currTokType == TokenType::FLOAT_LIT || currTokType == TokenType::SC ||
        currTokType == TokenType::LBRA || currTokType == TokenType::WHILE ||
        currTokType == TokenType::IF || currTokType == TokenType::ELSE ||
        currTokType == TokenType::RETURN) { // FIRST(stmt)
      // expand by stmt_list ::= stmt stmt_list_prime
      auto stmt = ParseStmt(lexer);
      if (stmt) {
        stmt_list.push_back(std::move(stmt));
      }
      auto stmt_prime = ParseStmtListPrime(lexer);
      for (unsigned i = 0; i < stmt_prime.size(); i++) {
        stmt_list.push_back(std::move(stmt_prime.at(i)));
      }

    } else if (currTokType == TokenType::RBRA) { // FOLLOW(stmt_list_prime)
      // expand by stmt_list_prime ::= ε
      // do nothing
    }
    return stmt_list; // note stmt_list can be empty as we can have empty
                      // blocks, etc.
  }

  // local_decls_prime ::= local_decl local_decls_prime
  //                    |  ε
  std::vector<std::unique_ptr<VarDeclAST>> ParseLocalDeclsPrime(Lexer &lexer) {
    std::vector<std::unique_ptr<VarDeclAST>>
        local_decls_prime; // vector of local decls

    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();

    if (currTokType == TokenType::INT_TOK ||
        currTokType == TokenType::FLOAT_TOK ||
        currTokType == TokenType::BOOL_TOK) { // FIRST(local_decl)
      auto local_decl = ParseLocalDecl(lexer);
      if (local_decl) {
        local_decls_prime.push_back(std::move(local_decl));
      }
      auto prime = ParseLocalDeclsPrime(lexer);
      for (unsigned i = 0; i < prime.size(); i++) {
        local_decls_prime.push_back(std::move(prime.at(i)));
      }
    } else if (currTokType == TokenType::MINUS ||
               currTokType == TokenType::NOT ||
               currTokType == TokenType::LPAR ||
               currTokType == TokenType::IDENT ||
               currTokType == TokenType::INT_LIT ||
               currTokType == TokenType::FLOAT_LIT ||
               currTokType == TokenType::BOOL_LIT ||
               currTokType == TokenType::SC || currTokType == TokenType::LBRA ||
               currTokType == TokenType::IF ||
               currTokType == TokenType::WHILE ||
               currTokType == TokenType::RETURN) { // FOLLOW(local_decls_prime)
      // expand by local_decls_prime ::=  ε
      // do nothing;
    } else {
      LogError(
          curr,
          "expected '-', '!', ('' , IDENT , STRING_LIT , INT_LIT , FLOAT_LIT, \
      BOOL_LIT, ';', '{', 'if', 'while', 'return' after local variable declaration\n");
    }

    return local_decls_prime;
  }

  // local_decl ::= var_type IDENT ";"
  // var_type ::= "int"
  //           |  "float"
  //           |  "bool"
  std::unique_ptr<VarDeclAST> ParseLocalDecl(Lexer &lexer) {
    std::string Type;
    std::string Name = "";
    std::unique_ptr<VarDeclAST> local_decl;

    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();
    if (currTokType == TokenType::INT_TOK ||
        currTokType == TokenType::FLOAT_TOK ||
        currTokType == TokenType::BOOL_TOK) { // FIRST(var_type)
      Token PrevTok = curr;
      curr = getNextToken(lexer).value(); // eat 'int' or 'float or 'bool'
      if (currTokType == TokenType::IDENT) {
        Type = PrevTok.getLexeme().value();
        Name = curr.getIdent().value(); // save the identifier name
        auto ident = std::make_unique<VariableASTnode>(curr, Name);
        local_decl = std::make_unique<VarDeclAST>(std::move(ident), Type);

        getNextToken(lexer); // eat 'IDENT'
        if (currTokType != TokenType::SC) {
          LogError(curr, "Expected ';' to end local variable declaration");
          return nullptr;
        }
        getNextToken(lexer); // eat ';'
        fprintf(stderr, "Parsed a local variable declaration\n");
      } else {
        LogError(curr, "expected identifier' in local variable declaration");
        return nullptr;
      }
    }
    return local_decl;
  }

  // local_decls ::= local_decl local_decls_prime
  std::vector<std::unique_ptr<VarDeclAST>> ParseLocalDecls(Lexer &lexer) {
    std::vector<std::unique_ptr<VarDeclAST>>
        local_decls; // vector of local decls

    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();
    if (currTokType == TokenType::INT_TOK ||
        currTokType == TokenType::FLOAT_TOK ||
        currTokType == TokenType::BOOL_TOK) { // FIRST(local_decl)

      auto local_decl = ParseLocalDecl(lexer);
      if (local_decl) {
        local_decls.push_back(std::move(local_decl));
      }
      auto local_decls_prime = ParseLocalDeclsPrime(lexer);
      for (unsigned i = 0; i < local_decls_prime.size(); i++) {
        local_decls.push_back(std::move(local_decls_prime.at(i)));
      }

    } else if (currTokType == TokenType::MINUS ||
               currTokType == TokenType::NOT ||
               currTokType == TokenType::LPAR ||
               currTokType == TokenType::IDENT ||
               currTokType == TokenType::INT_LIT ||
               currTokType == TokenType::RETURN ||
               currTokType == TokenType::FLOAT_LIT ||
               currTokType == TokenType::BOOL_LIT ||
               currTokType == TokenType::COMMA ||
               currTokType == TokenType::LBRA || currTokType == TokenType::IF ||
               currTokType == TokenType::WHILE) { // FOLLOW(local_decls)
                                                  // do nothing
    } else {
      LogError(
          curr,
          "expected '-', '!', '(' , IDENT , STRING_LIT , INT_LIT , FLOAT_LIT, \
        BOOL_LIT, ';', '{', 'if', 'while', 'return'");
    }

    return local_decls;
  }

  // parse block
  // block ::= "{" local_decls stmt_list "}"
  std::unique_ptr<ASTnode> ParseBlock(Lexer &lexer) {
    std::vector<std::unique_ptr<VarDeclAST>>
        local_decls;                                 // vector of local decls
    std::vector<std::unique_ptr<ASTnode>> stmt_list; // vector of statements

    getNextToken(lexer); // eat '{'

    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();
    local_decls = ParseLocalDecls(lexer);
    fprintf(stderr, "Parsed a set of local variable declaration\n");
    stmt_list = ParseStmtList(lexer);
    fprintf(stderr, "Parsed a list of statements\n");
    if (currTokType == TokenType::RBRA)
      getNextToken(lexer); // eat '}'
    else {                 // syntax error
      LogError(curr, "expected '}' , close body of block");
      return nullptr;
    }

    return std::make_unique<BlockAST>(std::move(local_decls),
                                      std::move(stmt_list));
  }

  // decl ::= var_decl
  //       |  fun_decl
  std::unique_ptr<ASTnode> ParseDecl(Lexer &lexer) {
    std::string IdName;
    std::vector<std::unique_ptr<ParamAST>> param_list;

    Token prevTok = curTok.value(); // to keep track of the type token

    auto prevTokType = prevTok.getTokenType();
    if (prevTokType == TokenType::VOID_TOK ||
        prevTokType == TokenType::INT_TOK ||
        prevTokType == TokenType::FLOAT_TOK ||
        prevTokType == TokenType::BOOL_TOK) {
      auto curr =
          getNextToken(lexer)
              .value(); // eat the VOID_TOK, INT_TOK, BOOL_TOK or FLOAT_TOK
      auto currTokType = curr.getTokenType();
      auto IdName = curr.getIdent().value(); // save the identifier name

      if (currTokType == TokenType::IDENT) {
        auto ident = std::make_unique<VariableASTnode>(curr, IdName);
        curr = getNextToken(lexer).value(); // eat the IDENT
        currTokType = curr.getTokenType();
        if (currTokType == TokenType::SC) { // found ';' then this is a global
                                            // variable declaration.
          getNextToken(lexer);              // eat ;
          fprintf(stderr, "Parsed a variable declaration\n");

          if (prevTokType != TokenType::VOID_TOK)
            return std::make_unique<GlobVarDeclAST>(
                std::move(ident), prevTok.getLexeme().value());
          else
            return LogError(
                prevTok, "Cannot have variable declaration with type 'void'");
        } else if (currTokType == TokenType::LPAR) { // found '(' then this is a
                                                     // function declaration.
          curr = getNextToken(lexer).value();        // eat (
          currTokType = curr.getTokenType();
          auto P = ParseParams(
              lexer); // parse the parameters, returns a vector of params
          // if (P.size() == 0) return nullptr;
          fprintf(stderr, "Parsed parameter list for function\n");

          if (currTokType != TokenType::RPAR) // syntax error
            return LogError(curr, "expected ')' in function declaration");

          curr = getNextToken(lexer).value(); // eat )
          currTokType = curr.getTokenType();
          if (currTokType != TokenType::LBRA) // syntax error
            return LogError(
                curr, "expected '{' in function declaration, function body");

          auto B = ParseBlock(lexer); // parse the function body
          if (!B)
            return nullptr;
          else
            fprintf(stderr, "Parsed block of statements in function\n");

          // now create a Function prototype
          // create a Function body
          // put these to together
          // and return a std::unique_ptr<FunctionDeclAST>
          fprintf(stderr, "Parsed a function declaration\n");

          auto Proto = std::make_unique<FunctionPrototypeAST>(
              IdName, prevTok.getIdent().value(), std::move(P));
          return std::make_unique<FunctionDeclAST>(std::move(Proto),
                                                   std::move(B));
        } else
          return LogError(curr, "expected ';' or ('");
      } else
        return LogError(curr, "expected an identifier");

    } else
      LogError(
          prevTok,
          "expected 'void', 'int' or 'float' or EOF token"); // syntax error

    return nullptr;
  }

  // decl_list_prime ::= decl decl_list_prime
  //                  |  ε
  void ParseDeclListPrime(Lexer &lexer) {
    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();
    if (currTokType == TokenType::VOID_TOK ||
        currTokType == TokenType::INT_TOK ||
        currTokType == TokenType::FLOAT_TOK ||
        currTokType == TokenType::BOOL_TOK) { // FIRST(decl)

      if (auto decl = ParseDecl(lexer)) {
        fprintf(stderr,
                "Parsed a top-level variable or function declaration\n");
        if (auto *DeclIR = decl->codegen()) {
          DeclIR->print(llvm::errs());
          fprintf(stderr, "\n");
        }
      }
      ParseDeclListPrime(lexer);
    } else if (currTokType == TokenType::EOF_TOK) { // FOLLOW(decl_list_prime)
      // expand by decl_list_prime ::= ε
      // do nothing
    } else { // syntax error
      LogError(curr, "expected 'void', 'int', 'bool' or 'float' or EOF token");
    }
  }

  // decl_list ::= decl decl_list_prime
  void ParseDeclList(Lexer &lexer) {
    auto decl = ParseDecl(lexer);
    if (decl) {
      fprintf(stderr, "Parsed a top-level variable or function declaration\n");
      if (auto *DeclIR = decl->codegen()) {
        DeclIR->print(llvm::errs());
        fprintf(stderr, "\n");
      }
      ParseDeclListPrime(lexer);
    }
  }

  // extern ::= "extern" type_spec IDENT "(" params ")" ";"
  std::unique_ptr<FunctionPrototypeAST> ParseExtern(Lexer &lexer) {
    std::string IdName;
    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();
    if (currTokType == TokenType::EXTERN) {
      curr = getNextToken(lexer).value(); // eat the EXTERN
      currTokType = curr.getTokenType();

      if (currTokType == TokenType::VOID_TOK ||
          currTokType == TokenType::INT_TOK ||
          currTokType == TokenType::FLOAT_TOK ||
          currTokType == TokenType::BOOL_TOK) {

        auto PrevTok = curr; // to keep track of the type token
        curr = getNextToken(lexer)
                   .value(); // eat the VOID_TOK, INT_TOK, BOOL_TOK or FLOAT_TOK
        currTokType = curr.getTokenType();

        if (currTokType == TokenType::IDENT) {
          IdName = curr.getIdent().value(); // save the identifier name
          auto ident = std::make_unique<VariableASTnode>(curr, IdName);
          curr = getNextToken(lexer).value(); // eat the IDENT
          currTokType = curr.getTokenType();

          if (currTokType == TokenType::LPAR) { // found '(' - this is an extern
                                                // function declaration.
            curr = getNextToken(lexer).value(); // eat (
            currTokType = curr.getTokenType();

            auto P = ParseParams(lexer); // parse the parameters, returns a
                                         // vector of params
            if (P.size() == 0)
              return nullptr;
            else
              fprintf(stderr, "Parsed parameter list for external function\n");

            if (currTokType != TokenType::RPAR) // syntax error
              return LogErrorP(
                  curr, "expected ')' in closing extern function declaration");

            curr = getNextToken(lexer).value(); // eat )
            currTokType = curr.getTokenType();

            if (currTokType == TokenType::SC) {
              getNextToken(lexer); // eat ";"
              auto Proto = std::make_unique<FunctionPrototypeAST>(
                  IdName, PrevTok.getLexeme().value(), std::move(P));
              return Proto;
            } else
              return LogErrorP(curr, "expected ;' in ending extern function "
                                     "declaration statement");
          } else
            return LogErrorP(curr,
                             "expected (' in extern function declaration");
        }

      } else
        LogErrorP(curr,
                  "expected 'void', 'int' or 'float' in extern function "
                  "declaration\n"); // syntax error
    }

    return nullptr;
  }

  // extern_list_prime ::= extern extern_list_prime
  //                   |  ε
  void ParseExternListPrime(Lexer &lexer) {
    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();
    if (currTokType == TokenType::EXTERN) { // FIRST(extern)
      if (auto Extern = ParseExtern(lexer)) {
        fprintf(stderr,
                "Parsed a top-level external function declaration -- 2\n");
        if (auto *ExternIR = Extern->codegen()) {
          ExternIR->print(llvm::errs());
          fprintf(stderr, "\n");
        }
      }
      ParseExternListPrime(lexer);
    } else if (currTokType == TokenType::VOID_TOK ||
               currTokType == TokenType::INT_TOK ||
               currTokType == TokenType::FLOAT_TOK ||
               currTokType ==
                   TokenType::BOOL_TOK) { // FOLLOW(extern_list_prime)
      // expand by decl_list_prime ::= ε
      // do nothing
    } else { // syntax error
      LogError(curr, "expected 'extern' or 'void',  'int' ,  'float',  'bool'");
    }
  }

  // extern_list ::= extern extern_list_prime
  void ParseExternList(Lexer &lexer) {
    auto Extern = ParseExtern(lexer);
    auto curr = curTok.value();
    auto currTokType = curr.getTokenType();
    if (Extern) {
      fprintf(stderr,
              "Parsed a top-level external function declaration -- 1\n");
      if (auto *ExternIR = Extern->codegen()) {
        ExternIR->print(llvm::errs());
        fprintf(stderr, "\n");
      }
      // fprintf(stderr, "Current token: %s \n", CurTok.lexeme.c_str());
      if (currTokType == TokenType::EXTERN)
        ParseExternListPrime(lexer);
    }
  }

  // program ::= extern_list decl_list
  void parser(Lexer &lexer) {
    auto currTokType = curTok.value().getTokenType();
    if (currTokType == TokenType::EOF_TOK)
      return;
    ParseExternList(lexer);
    if (currTokType == TokenType::EOF_TOK)
      return;
    ParseDeclList(lexer);
    if (currTokType == TokenType::EOF_TOK)
      return;
  }
};
} // namespace mccomp
