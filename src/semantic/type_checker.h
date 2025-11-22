#pragma once

#include <ast/ast.h>

namespace mccomp {

class TypeChecker final : public ASTVisitor {
public:
  auto visitTranslationUnit(TranslationUnit &) -> void override;
  auto visitFunctionDecl(FunctionDecl &) -> void override;
  auto visitVarDecl(VarDecl &) -> void override;
  auto visitParmVarDecl(ParmVarDecl &) -> void override;
  auto visitCompoundStmt(CompoundStmt &) -> void override;
  auto visitIfStmt(IfStmt &) -> void override;
  auto visitWhileStmt(WhileStmt &) -> void override;
  auto visitReturnStmt(ReturnStmt &) -> void override;
  auto visitDeclStmt(DeclStmt &) -> void override;
  auto visitCallExpr(CallExpr &) -> void override;
  auto visitParenExpr(ParenExpr &) -> void override;
  auto visitDeclRefExpr(DeclRefExpr &) -> void override;
  auto visitArraySubscriptExpr(ArraySubscriptExpr &) -> void override;
  auto visitImplicitCastExpr(ImplicitCastExpr &) -> void override;
  auto visitBinaryOperator(BinaryOperator &) -> void override;
  auto visitUnaryOperator(UnaryOperator &) -> void override;
  auto visitIntegerLiteral(IntegerLiteral &) -> void override;
  auto visitFloatLiteral(FloatLiteral &) -> void override;
  auto visitBoolLiteral(BoolLiteral &) -> void override;
};

} // namespace mccomp
