#include "ast/_internal/ctx.h"
#include "ast/printer.h"
#include "error/error.h"
#include "parser/_internal/util.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <string.h>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#include <fmt/format.h>
#include <lexer/lexer.h>
#include <parser/parser.h>
#include <semantic/type_checker.h>
#include <tokens/tokens.h>

using namespace llvm;
using namespace llvm::sys;

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;

//===----------------------------------------------------------------------===//
// AST Printer
//===----------------------------------------------------------------------===//

// void IntASTnode::display(int tabs) {
//   printf("%s\n",getType().c_str());
// }

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
  auto parser = mccomp::Parser();

  // get the first token
  auto currentToken = parser.getNextToken(lexer);
  while (currentToken.getTokenType() != mccomp::TokenType::EOF_TOK) {
    std::cerr << "curr: ";
    printToken(currentToken);
    currentToken = parser.getNextToken(lexer);
  }
  fprintf(stderr, "Lexer Finished\n");

  // Make the module, which holds all the code.
  TheModule = std::make_unique<Module>("mini-c", TheContext);

  // Run the parser now.
  auto lexerAst = mccomp::Lexer(programName, argv[1]);
  auto astContext = mccomp::ASTContext();
  auto ast = parser.parseProgram(lexerAst, astContext);
  fprintf(stderr, "Parsing Finished\n");
  if (!ast) {
    fmt::println("{}", ast.error().to_string());
    return -2;
  }

  auto astPrinter = mccomp::ASTPrinter();
  auto typeChecker = mccomp::TypeChecker(astContext);
  ast.value()->accept(typeChecker);

  if (!typeChecker.isSuccess()) {
    fmt::println("unsuccessful type check");
  }

  ast.value()->accept(astPrinter);

  printf(
      "********************* FINAL IR (begin) ****************************\n");
  // Print out all of the generated code into a file called output.ll
  // printf("%s\n", argv[1]);
  auto Filename = "output.ll";
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }
  // TheModule->print(errs(), nullptr); // print IR to terminal
  TheModule->print(dest, nullptr);
  printf(
      "********************* FINAL IR (end) ******************************\n");

  return 0;
}
