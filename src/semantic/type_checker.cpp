#include <semantic/type_checker.h>

namespace mccomp {

auto TypeChecker::visitTranslationUnit(TranslationUnit &node) -> void {}
auto TypeChecker::visitFunctionDecl(FunctionDecl &node) -> void {}
auto TypeChecker::visitVarDecl(VarDecl &node) -> void {}
auto TypeChecker::visitParmVarDecl(ParmVarDecl &node) -> void {}
auto TypeChecker::visitCompoundStmt(CompoundStmt &node) -> void {}
auto TypeChecker::visitIfStmt(IfStmt &node) -> void {}
auto TypeChecker::visitWhileStmt(WhileStmt &node) -> void {}
auto TypeChecker::visitReturnStmt(ReturnStmt &node) -> void {}
auto TypeChecker::visitDeclStmt(DeclStmt &node) -> void {}
auto TypeChecker::visitCallExpr(CallExpr &node) -> void {}
auto TypeChecker::visitParenExpr(ParenExpr &node) -> void {}
auto TypeChecker::visitDeclRefExpr(DeclRefExpr &node) -> void {}
auto TypeChecker::visitArraySubscriptExpr(ArraySubscriptExpr &node) -> void {}
auto TypeChecker::visitImplicitCastExpr(ImplicitCastExpr &node) -> void {}
auto TypeChecker::visitBinaryOperator(BinaryOperator &node) -> void {}
auto TypeChecker::visitUnaryOperator(UnaryOperator &node) -> void {}
auto TypeChecker::visitIntegerLiteral(IntegerLiteral &node) -> void {}
auto TypeChecker::visitFloatLiteral(FloatLiteral &node) -> void {}
auto TypeChecker::visitBoolLiteral(BoolLiteral &node) -> void {}

} // namespace mccomp
