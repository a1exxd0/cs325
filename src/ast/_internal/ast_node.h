#pragma once

#include "fmt/compile.h"
#include <cstdio>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <string>
#include <tokens/tokens.h>
#include <vector>

namespace mccomp {

class ASTVisitor;
class ConstASTVisitor;

class SourceLocation {
public:
  std::size_t startLineNo;
  std::size_t startColumnNo;
  std::size_t endLineNo;
  std::size_t endColumnNo;
  std::optional<std::string_view> fileName;

  SourceLocation() = delete;
  SourceLocation(std::size_t startLineNo, std::size_t startColumnNo,
                 std::size_t endLineNo, std::size_t endColumnNo)
      : startLineNo(startLineNo), startColumnNo(startColumnNo),
        endLineNo(endLineNo), endColumnNo(endColumnNo) {}
  SourceLocation(std::size_t startLineNo, std::size_t startColumnNo,
                 std::size_t endLineNo, std::size_t endColumnNo,
                 std::string_view fileName)
      : startLineNo(startLineNo), startColumnNo(startColumnNo),
        endLineNo(endLineNo), endColumnNo(endColumnNo), fileName(fileName) {}
  SourceLocation(const Token &token, std::string_view fileName)
      : startLineNo(token.getLineNo()), startColumnNo(token.getColumnNo()),
        endLineNo(token.getLineNo()),
        endColumnNo(token.getColumnNo() + token.getLexeme().size() - 1),
        fileName(fileName) {}
  SourceLocation(const Token &leftToken, const Token &rightToken,
                 std::string_view fileName)
      : startLineNo(leftToken.getLineNo()),
        startColumnNo(leftToken.getColumnNo()),
        endLineNo(rightToken.getLineNo()),
        endColumnNo(rightToken.getColumnNo() + rightToken.getLexeme().size() -
                    1),
        fileName(fileName) {}

  auto to_string() const -> std::string {
    return fmt::format(FMT_COMPILE("{}:{}:{}"), fileName.value(), startLineNo,
                       startColumnNo);
  }
};

/// ASTnode - Base class for all AST nodes.
class ASTNode {
public:
  // Heavy inspiration from `clang` implementation of node classes.
  enum NodeKind {
    // `program` in grammar
    NK_TranslationUnit,

    NK_ExternDecl,
    NK_FunctionDecl,
    NK_VarDecl,
    NK_ParmVarDecl,

    // `block` in grammar
    NK_CompoundStmt,
    NK_IfStmt,
    NK_WhileStmt,
    NK_ReturnStmt,
    NK_DeclStmt,

    NK_CallExpr,
    NK_ParenExpr,
    NK_DeclRefExpr,
    NK_ArraySubscriptExpr,
    NK_ImplicitCastExpr,

    NK_BinaryOperator,
    NK_UnaryOperator,

    NK_IntegerLiteral,
    NK_FloatLiteral,
    NK_BoolLiteral,
  };

protected:
  NodeKind nodeKind;
  SourceLocation sourceLocation;
  explicit ASTNode(NodeKind nodeKind, SourceLocation sourceLocation)
      : nodeKind(nodeKind), sourceLocation(std::move(sourceLocation)) {}

public:
  ASTNode(const ASTNode &) = delete;
  ASTNode &operator=(const ASTNode &) = delete;
  ASTNode(ASTNode &&) = default;
  ASTNode &operator=(ASTNode &&) = default;
  virtual ~ASTNode() {}

  auto getKind() const -> NodeKind { return nodeKind; }
  auto getLocation() const -> const SourceLocation & { return sourceLocation; }

  virtual auto accept(ASTVisitor &visitor) -> void = 0;
  virtual auto accept(ConstASTVisitor &visitor) const -> void = 0;
  virtual auto getChildren() -> std::vector<ASTNode *> = 0;
  virtual auto getChildren() const -> std::vector<const ASTNode *> = 0;
  static auto classof(const ASTNode *) -> bool { return true; }
};

auto to_string(ASTNode::NodeKind kind) -> std::string;

} // namespace mccomp
