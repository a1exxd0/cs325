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
  virtual ~ASTVisitor();
  virtual auto visitTranslationUnit(TranslationUnit &) -> void = 0;
  virtual auto visitFunctionDecl(FunctionDecl &) -> void = 0;
  virtual auto visitVarDecl(VarDecl &) -> void = 0;
  virtual auto visitParmVarDecl(ParmVarDecl &) -> void = 0;
  virtual auto visitCompoundStmt(CompoundStmt &) -> void = 0;
  virtual auto visitIfStmt(IfStmt &) -> void = 0;
  virtual auto visitWhileStmt(WhileStmt &) -> void = 0;
  virtual auto visitReturnStmt(ReturnStmt &) -> void = 0;
  virtual auto visitDeclStmt(DeclStmt &) -> void = 0;
  virtual auto visitCallExpr(CallExpr &) -> void = 0;
  virtual auto visitParenExpr(ParenExpr &) -> void = 0;
  virtual auto visitDeclRefExpr(DeclRefExpr &) -> void = 0;
  virtual auto visitArraySubscriptExpr(ArraySubscriptExpr &) -> void = 0;
  virtual auto visitImplicitCastExpr(ImplicitCastExpr &) -> void = 0;
  virtual auto visitBinaryOperator(BinaryOperator &) -> void = 0;
  virtual auto visitUnaryOperator(UnaryOperator &) -> void = 0;
  virtual auto visitIntegerLiteral(IntegerLiteral &) -> void = 0;
  virtual auto visitFloatLiteral(FloatLiteral &) -> void = 0;
  virtual auto visitBoolLiteral(BoolLiteral &) -> void = 0;
};

class ConstASTVisitor {
public:
  virtual ~ConstASTVisitor();
  virtual auto visitTranslationUnit(const TranslationUnit &) -> void = 0;
  virtual auto visitFunctionDecl(const FunctionDecl &) -> void = 0;
  virtual auto visitVarDecl(const VarDecl &) -> void = 0;
  virtual auto visitParmVarDecl(const ParmVarDecl &) -> void = 0;
  virtual auto visitCompoundStmt(const CompoundStmt &) -> void = 0;
  virtual auto visitIfStmt(const IfStmt &) -> void = 0;
  virtual auto visitWhileStmt(const WhileStmt &) -> void = 0;
  virtual auto visitReturnStmt(const ReturnStmt &) -> void = 0;
  virtual auto visitDeclStmt(const DeclStmt &) -> void = 0;
  virtual auto visitCallExpr(const CallExpr &) -> void = 0;
  virtual auto visitParenExpr(const ParenExpr &) -> void = 0;
  virtual auto visitDeclRefExpr(const DeclRefExpr &) -> void = 0;
  virtual auto visitArraySubscriptExpr(const ArraySubscriptExpr &) -> void = 0;
  virtual auto visitImplicitCastExpr(const ImplicitCastExpr &) -> void = 0;
  virtual auto visitBinaryOperator(const BinaryOperator &) -> void = 0;
  virtual auto visitUnaryOperator(const UnaryOperator &) -> void = 0;
  virtual auto visitIntegerLiteral(const IntegerLiteral &) -> void = 0;
  virtual auto visitFloatLiteral(const FloatLiteral &) -> void = 0;
  virtual auto visitBoolLiteral(const BoolLiteral &) -> void = 0;
};
} // namespace mccomp
