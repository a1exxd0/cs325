#include <ast/printer.h>
#include <colors/colors.h>
#include <fmt/format.h>

namespace mccomp {

auto ASTPrinter::visitTranslationUnit(const TranslationUnit &node) -> void {
  auto fmtStr = fmt::format(
      FMT_COMPILE("{}TranslationUnitDecl {}{}{} <{}<invalid sloc>{}> "
                  "{}<invalid sloc>{}"),
      text_colors::GREEN, text_colors::YELLOW, static_cast<const void *>(&node),
      text_colors::RESET, text_colors::YELLOW, text_colors::RESET,
      text_colors::YELLOW, text_colors::RESET);
}

auto ASTPrinter::visitFunctionDecl(const FunctionDecl &node) -> void {}
auto ASTPrinter::visitVarDecl(const VarDecl &node) -> void {}
auto ASTPrinter::visitParmVarDecl(const ParmVarDecl &node) -> void {}
auto ASTPrinter::visitCompoundStmt(const CompoundStmt &node) -> void {}
auto ASTPrinter::visitIfStmt(const IfStmt &node) -> void {}
auto ASTPrinter::visitWhileStmt(const WhileStmt &node) -> void {}
auto ASTPrinter::visitReturnStmt(const ReturnStmt &node) -> void {}
auto ASTPrinter::visitDeclStmt(const DeclStmt &node) -> void {}
auto ASTPrinter::visitCallExpr(const CallExpr &node) -> void {}
auto ASTPrinter::visitParenExpr(const ParenExpr &node) -> void {}
auto ASTPrinter::visitDeclRefExpr(const DeclRefExpr &node) -> void {}
auto ASTPrinter::visitArraySubscriptExpr(const ArraySubscriptExpr &node)
    -> void {}
auto ASTPrinter::visitImplicitCastExpr(const ImplicitCastExpr &node) -> void {}
auto ASTPrinter::visitBinaryOperator(const BinaryOperator &node) -> void {}
auto ASTPrinter::visitUnaryOperator(const UnaryOperator &node) -> void {}
auto ASTPrinter::visitIntegerLiteral(const IntegerLiteral &node) -> void {}
auto ASTPrinter::visitFloatLiteral(const FloatLiteral &node) -> void {}
auto ASTPrinter::visitBoolLiteral(const BoolLiteral &node) -> void {}

} // namespace mccomp
