#pragma once

namespace mccomp {

class TranslationUnit;
class ExternDecl;
class FunctionDecl;
class VarDecl;
class ParmVarDecl;
class CompoundStmt;
class IfStmt;
class WhileStmt;
class ReturnStmt;
class DeclStmt;
class CallExpr;
class ParenExpr;
class DeclRefExpr;
class ArraySubscriptExpr;
class ImplicitCastExpr;
class BinaryOperator;
class UnaryOperator;
class IntegerLiteral;
class FloatLiteral;
class BoolLiteral;

class ASTVisitor {
public:
  virtual ~ASTVisitor() = default;

  virtual auto visitTranslationUnit(TranslationUnit &) -> void;
  virtual auto visitExternDecl(ExternDecl &) -> void;
  virtual auto visitFunctionDecl(FunctionDecl &) -> void;
  virtual auto visitVarDecl(VarDecl &) -> void;
  virtual auto visitParmVarDecl(ParmVarDecl &) -> void;
  virtual auto visitCompoundStmt(CompoundStmt &) -> void;
  virtual auto visitIfStmt(IfStmt &) -> void;
  virtual auto visitWhileStmt(WhileStmt &) -> void;
  virtual auto visitReturnStmt(ReturnStmt &) -> void;
  virtual auto visitDeclStmt(DeclStmt &) -> void;
  virtual auto visitCallExpr(CallExpr &) -> void;
  virtual auto visitParenExpr(ParenExpr &) -> void;
  virtual auto visitDeclRefExpr(DeclRefExpr &) -> void;
  virtual auto visitArraySubscriptExpr(ArraySubscriptExpr &) -> void;
  virtual auto visitImplicitCastExpr(ImplicitCastExpr &) -> void;
  virtual auto visitBinaryOperator(BinaryOperator &) -> void;
  virtual auto visitUnaryOperator(UnaryOperator &) -> void;
  virtual auto visitIntegerLiteral(IntegerLiteral &) -> void;
  virtual auto visitFloatLiteral(FloatLiteral &) -> void;
  virtual auto visitBoolLiteral(BoolLiteral &) -> void;
};

class ConstASTVisitor {
public:
  virtual ~ConstASTVisitor() = default;

  virtual auto visitTranslationUnit(const TranslationUnit &) -> void;
  virtual auto visitExternDecl(const ExternDecl &) -> void;
  virtual auto visitFunctionDecl(const FunctionDecl &) -> void;
  virtual auto visitVarDecl(const VarDecl &) -> void;
  virtual auto visitParmVarDecl(const ParmVarDecl &) -> void;
  virtual auto visitCompoundStmt(const CompoundStmt &) -> void;
  virtual auto visitIfStmt(const IfStmt &) -> void;
  virtual auto visitWhileStmt(const WhileStmt &) -> void;
  virtual auto visitReturnStmt(const ReturnStmt &) -> void;
  virtual auto visitDeclStmt(const DeclStmt &) -> void;
  virtual auto visitCallExpr(const CallExpr &) -> void;
  virtual auto visitParenExpr(const ParenExpr &) -> void;
  virtual auto visitDeclRefExpr(const DeclRefExpr &) -> void;
  virtual auto visitArraySubscriptExpr(const ArraySubscriptExpr &) -> void;
  virtual auto visitImplicitCastExpr(const ImplicitCastExpr &) -> void;
  virtual auto visitBinaryOperator(const BinaryOperator &) -> void;
  virtual auto visitUnaryOperator(const UnaryOperator &) -> void;
  virtual auto visitIntegerLiteral(const IntegerLiteral &) -> void;
  virtual auto visitFloatLiteral(const FloatLiteral &) -> void;
  virtual auto visitBoolLiteral(const BoolLiteral &) -> void;
};
} // namespace mccomp
