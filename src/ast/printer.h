#pragma once

#include <ast/ast.h>

namespace mccomp {

class ASTPrinter : ConstASTVisitor {
  std::size_t currDepth;

private:
  auto visitTranslationUnit(const TranslationUnit &node) -> void;
  auto visitFunctionDecl(const FunctionDecl &node) -> void;
  auto visitVarDecl(const VarDecl &node) -> void;
  auto visitParmVarDecl(const ParmVarDecl &node) -> void;
  auto visitCompoundStmt(const CompoundStmt &node) -> void;
  auto visitIfStmt(const IfStmt &node) -> void;
  auto visitWhileStmt(const WhileStmt &node) -> void;
  auto visitReturnStmt(const ReturnStmt &node) -> void;
  auto visitDeclStmt(const DeclStmt &node) -> void;
  auto visitCallExpr(const CallExpr &node) -> void;
  auto visitParenExpr(const ParenExpr &node) -> void;
  auto visitDeclRefExpr(const DeclRefExpr &node) -> void;
  auto visitArraySubscriptExpr(const ArraySubscriptExpr &node) -> void;
  auto visitImplicitCastExpr(const ImplicitCastExpr &node) -> void;
  auto visitBinaryOperator(const BinaryOperator &node) -> void;
  auto visitUnaryOperator(const UnaryOperator &node) -> void;
  auto visitIntegerLiteral(const IntegerLiteral &node) -> void;
  auto visitFloatLiteral(const FloatLiteral &node) -> void;
  auto visitBoolLiteral(const BoolLiteral &node) -> void;
};

} // namespace mccomp
