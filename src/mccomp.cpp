#include "error/clang_error.h"
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
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <string.h>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#include <lexer/lexer.h>
#include <parser/parser.h>
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

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  auto lexer = mccomp::Lexer(argv[1]);
  auto parser = mccomp::Parser();

  // get the first token
  auto currentToken = parser.getNextToken(lexer).value();
  while (currentToken.getTokenType() != mccomp::TokenType::EOF_TOK) {
    if (currentToken.getTokenType() == mccomp::TokenType::INT_LIT) {
      fprintf(stderr, "Token: %d with type %d\n", currentToken.getInt().value(),
              currentToken.getTokenType());
    } else if (currentToken.getTokenType() == mccomp::TokenType::FLOAT_LIT) {
      fprintf(stderr, "Token: %f with type %d\n",
              currentToken.getFloat().value(), currentToken.getTokenType());
    } else {
      fprintf(stderr, "Token: %s with type %d\n",
              currentToken.getLexeme().value().c_str(),
              currentToken.getTokenType());
    }
    currentToken = parser.getNextToken(lexer).value();
  }
  fprintf(stderr, "Lexer Finished\n");

  // Make the module, which holds all the code.
  TheModule = std::make_unique<Module>("mini-c", TheContext);

  // Run the parser now.

  /* UNCOMMENT : Task 2 - Parser
   * parser();
   * fprintf(stderr, "Parsing Finished\n");
   */

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

  auto demoError = mccomp::ClangError(
      mccomp::ClangErrorSeverity::ERROR, "custom_tests/hello.c", 1, 18,
      {22, 28}, "failed to convert integer lexeme to int");

  std::cout << demoError.to_string() << std::endl;

  return 0;
}
