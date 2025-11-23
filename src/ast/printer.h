#pragma once

#include <ast/ast.h>
#include <deque>

namespace mccomp {

class ASTPrinter final : public ConstASTVisitor {
  std::string_view currFile;
  std::size_t currLine = 0;
  std::size_t currCol = 0;
  std::size_t currIndent = 0;
  std::deque<std::size_t> activeBlocks;
  bool lastChild = true;

  auto formatRegion(const SourceLocation &loc) const -> std::string;
  auto formatRegionWithIdent(const SourceLocation &loc,
                             const Token &ident) const -> std::string;

  auto formatPrefix() const -> std::string;
  auto formatType(const Type *type) const -> std::string;
  auto visitChildren(const ASTNode &node) -> void;

public:
  ASTPrinter() = default;
  auto visitTranslationUnit(const TranslationUnit &node) -> void override;
  auto visitFunctionDecl(const FunctionDecl &node) -> void override;
  auto visitVarDecl(const VarDecl &node) -> void override;
  auto visitParmVarDecl(const ParmVarDecl &node) -> void override;
  auto visitCompoundStmt(const CompoundStmt &node) -> void override;
  auto visitIfStmt(const IfStmt &node) -> void override;
  auto visitWhileStmt(const WhileStmt &node) -> void override;
  auto visitReturnStmt(const ReturnStmt &node) -> void override;
  auto visitDeclStmt(const DeclStmt &node) -> void override;
  auto visitCallExpr(const CallExpr &node) -> void override;
  auto visitParenExpr(const ParenExpr &node) -> void override;
  auto visitDeclRefExpr(const DeclRefExpr &node) -> void override;
  auto visitArraySubscriptExpr(const ArraySubscriptExpr &node) -> void override;
  auto visitImplicitCastExpr(const ImplicitCastExpr &node) -> void override;
  auto visitBinaryOperator(const BinaryOperator &node) -> void override;
  auto visitUnaryOperator(const UnaryOperator &node) -> void override;
  auto visitIntegerLiteral(const IntegerLiteral &node) -> void override;
  auto visitFloatLiteral(const FloatLiteral &node) -> void override;
  auto visitBoolLiteral(const BoolLiteral &node) -> void override;
};

} // namespace mccomp
