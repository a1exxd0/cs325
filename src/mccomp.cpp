#include "ast/printer.h"
#include "codegen/codegen.h"
#include "error/error.h"
#include "llvm/Support/FileSystem.h"
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <string>
#include <system_error>

#include <fmt/format.h>
#include <lexer/lexer.h>
#include <parser/parser.h>
#include <semantic/type_checker.h>
#include <tokens/tokens.h>

using namespace llvm;
using namespace llvm::sys;

//===----------------------------------------------------------------------===//
// AST Printer
//===----------------------------------------------------------------------===//

auto printToken(const mccomp::Token &token) -> void {
  if (token.getTokenType() == mccomp::TokenType::INT_LIT) {
    fprintf(stderr, "Token: %d with type %d\n", token.asInt(),
            token.getTokenType());
  } else if (token.getTokenType() == mccomp::TokenType::FLOAT_LIT) {
    fprintf(stderr, "Token: %f with type %d\n", token.asFloat(),
            token.getTokenType());
  } else {
    fprintf(stderr, "Token: %s with type %d\n", token.getLexeme().c_str(),
            token.getTokenType());
  }
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  const auto programName = std::filesystem::path(argv[0]).filename().string();
  auto lexer = mccomp::Lexer(programName, argv[1]);
  printf("********************* LEXER (begin) ****************************\n");

  // get the first token
  auto currentToken = lexer.getToken();
  while (currentToken.getTokenType() != mccomp::TokenType::EOF_TOK) {
    std::cerr << "curr: ";
    printToken(currentToken);
    currentToken = lexer.getToken();
  }
  printf("********************* LEXER (end) ****************************\n");

  printf("********************* PARSER (begin) ****************************\n");
  // Run the parser now.
  auto lexerAst = mccomp::Lexer(programName, argv[1]);
  auto astContext = mccomp::ASTContext();
  auto parser = mccomp::Parser();
  auto astExp = parser.parseProgram(lexerAst, astContext);
  if (!astExp) {
    fmt::println("{}", astExp.error().to_string());
    return -2;
  }

  auto ast = std::move(astExp.value());
  auto astPrinter = mccomp::ASTPrinter();
  auto typeChecker = mccomp::TypeChecker(astContext);
  ast->accept(typeChecker);
  ast->accept(astPrinter);

  printf("********************* PARSER (end) ****************************\n");
  printf(
      "********************* FINAL IR (begin) ****************************\n");

  if (typeChecker.isSuccess()) {
    auto codegen = mccomp::CodeGen("mccomp");
    ast->accept(codegen);
    auto Filename = "output.ll";
    std::error_code EC;
    raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);
    codegen.print(dest);

    if (EC) {
      errs() << "Could not open file: " << EC.message();
      return 1;
    }
  }

  printf(
      "********************* FINAL IR (end) ******************************\n");

  return 0;
}
