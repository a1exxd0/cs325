#include <ast/printer.h>
#include <colors/colors.h>
#include <fmt/format.h>

auto formatAddress(const mccomp::ASTNode *node) -> std::string {
  return fmt::format(FMT_COMPILE("{}{}{}"), mccomp::text_colors::YELLOW,
                     static_cast<const void *>(node),
                     mccomp::text_colors::RESET);
}

namespace mccomp {

auto ASTPrinter::formatPrefix() const -> std::string {
  auto prefix = std::string(currIndent, ' ');
  if (currIndent < 2)
    return prefix;

  if (lastChild) {
    prefix[currIndent - 2] = '`';
  } else {
    prefix[currIndent - 2] = '|';
  }
  prefix[currIndent - 1] = '-';

  for (const auto &block : activeBlocks) {
    if (block < currIndent)
      prefix[block] = '|';
  }

  return fmt::format(FMT_COMPILE("{}{}{}"), text_colors::BLUE, prefix,
                     text_colors::RESET);
}

auto ASTPrinter::formatRegion(const SourceLocation &sourceLocation) const
    -> std::string {
  if (this->currFile == sourceLocation.fileName &&
      this->currLine == sourceLocation.startLineNo) {
    if (sourceLocation.startLineNo == sourceLocation.endLineNo) {
      return fmt::format(FMT_COMPILE("{}<{}col:{}, col:{}{}>"),
                         text_colors::RESET, text_colors::YELLOW,
                         sourceLocation.startColumnNo,
                         sourceLocation.endColumnNo, text_colors::RESET);
    } else {
      return fmt::format(FMT_COMPILE("{}<{}col:{}, line:{}:{}{}>"),
                         text_colors::RESET, text_colors::YELLOW,
                         sourceLocation.startColumnNo, sourceLocation.endLineNo,
                         sourceLocation.endColumnNo, text_colors::RESET);
    }
  } else if (this->currFile == sourceLocation.fileName) {
    if (sourceLocation.startLineNo == sourceLocation.endLineNo) {
      return fmt::format(FMT_COMPILE("{}<{}line:{}:{}, col:{}{}>"),
                         text_colors::RESET, text_colors::YELLOW,
                         sourceLocation.startLineNo,
                         sourceLocation.startColumnNo,
                         sourceLocation.endColumnNo, text_colors::RESET);
    } else {
      return fmt::format(FMT_COMPILE("{}<{}line:{}:{}, line:{}:{}{}>"),
                         text_colors::RESET, text_colors::YELLOW,
                         sourceLocation.startLineNo,
                         sourceLocation.startColumnNo, sourceLocation.endLineNo,
                         sourceLocation.endColumnNo, text_colors::RESET);
    }
  } else {
    if (sourceLocation.startLineNo == sourceLocation.endLineNo) {
      return fmt::format(FMT_COMPILE("{}<{}{}:{}:{}, col:{}{}>"),
                         text_colors::RESET, text_colors::YELLOW,
                         ((!sourceLocation.fileName.has_value())
                              ? "no-name"
                              : sourceLocation.fileName.value()),
                         sourceLocation.startLineNo,
                         sourceLocation.startColumnNo,
                         sourceLocation.endColumnNo, text_colors::RESET);
    } else {
      return fmt::format(FMT_COMPILE("{}<{}{}:{}:{}, line:{}:{}{}>"),
                         text_colors::RESET, text_colors::YELLOW,
                         ((!sourceLocation.fileName.has_value())
                              ? "no-name"
                              : sourceLocation.fileName.value()),
                         sourceLocation.startLineNo,
                         sourceLocation.startColumnNo, sourceLocation.endLineNo,
                         sourceLocation.endColumnNo, text_colors::RESET);
    }
  }
}

auto ASTPrinter::formatRegionWithIdent(const SourceLocation &loc,
                                       const Token &ident) const
    -> std::string {
  auto block = this->formatRegion(loc);
  if (static_cast<int>(loc.startLineNo) == ident.getLineNo()) {
    block += fmt::format(FMT_COMPILE(" {}col:{}{}"), text_colors::YELLOW,
                         ident.getColumnNo(), text_colors::RESET);
  } else {
    block +=
        fmt::format(FMT_COMPILE(" {}line:{}:{}{}"), text_colors::YELLOW,
                    ident.getLineNo(), ident.getColumnNo(), text_colors::RESET);
  }

  return block;
}

auto ASTPrinter::visitChildren(const ASTNode &node) -> void {
  this->activeBlocks.push_back(currIndent);
  this->currIndent += 2;
  auto oldLastChild = this->lastChild;
  this->lastChild = false;

  const auto children = node.getChildren();
  const auto n = static_cast<int>(children.size());
  for (auto i = 0; i < n; ++i) {
    if (i == (n - 1)) {
      this->lastChild = true;
      this->activeBlocks.erase(
          std::find(activeBlocks.begin(), activeBlocks.end(), currIndent - 2));
    }
    children[i]->accept(*this);
    this->lastChild = false;
  }

  this->currIndent -= 2;
  this->lastChild = oldLastChild;
}

auto ASTPrinter::visitTranslationUnit(const TranslationUnit &node) -> void {
  auto fmtStr =
      fmt::format(FMT_COMPILE("{}TranslationUnitDecl{} {} <{}<invalid sloc>{}> "
                              "{}<invalid sloc>{}"),
                  text_colors::GREEN, text_colors::RESET, formatAddress(&node),
                  text_colors::YELLOW, text_colors::RESET, text_colors::YELLOW,
                  text_colors::RESET);

  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
  fmt::println("{}", fmtStr);
  visitChildren(node);
}

auto ASTPrinter::visitFunctionDecl(const FunctionDecl &node) -> void {
  auto fmtStr = fmt::format(
      FMT_COMPILE("{}{}FunctionDecl{} {} {} {}{}{}"), formatPrefix(),
      text_colors::GREEN, text_colors::RESET, formatAddress(&node),
      formatRegionWithIdent(node.getLocation(), node.getIdent()),
      text_colors::BLUE, node.getName(), text_colors::RESET);

  fmt::println("{} {}", fmtStr, (node.getBody() == nullptr) ? "extern" : "");
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
  visitChildren(node);
}

auto ASTPrinter::visitVarDecl(const VarDecl &node) -> void {
  auto fmtStr =
      fmt::format(FMT_COMPILE("{}{}VarDecl{} {} {} {}{}{}"), formatPrefix(),
                  text_colors::GREEN, text_colors::RESET, formatAddress(&node),
                  formatRegionWithIdent(node.getLocation(), node.getIdent()),
                  text_colors::BLUE, node.getName(), text_colors::RESET);
  fmt::println("{}", fmtStr);
  if (node.getInit() == nullptr)
    return;

  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
  visitChildren(node);
}

auto ASTPrinter::visitParmVarDecl(const ParmVarDecl &node) -> void {
  auto fmtStr = fmt::format(
      FMT_COMPILE("{}{}ParmVarDecl{} {}{}{} {} {}{}{}"), formatPrefix(),
      text_colors::GREEN, text_colors::RESET, text_colors::YELLOW,
      static_cast<const void *>(&node), text_colors::RESET,
      formatRegionWithIdent(node.getLocation(), node.getIdent()),
      text_colors::BLUE, node.getName(), text_colors::RESET);
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
}

auto ASTPrinter::visitCompoundStmt(const CompoundStmt &node) -> void {
  auto fmtStr =
      fmt::format(FMT_COMPILE("{}{}CompoundStmt{} {} {}"), formatPrefix(),
                  text_colors::MAGNETA, text_colors::RESET,
                  formatAddress(&node), formatRegion(node.getLocation()));
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
  visitChildren(node);
}

auto ASTPrinter::visitIfStmt(const IfStmt &node) -> void {
  auto fmtStr =
      fmt::format(FMT_COMPILE("{}{}IfStmt{} {} {} {}"), formatPrefix(),
                  text_colors::MAGNETA, text_colors::RESET,
                  formatAddress(&node), formatRegion(node.getLocation()),
                  (node.getElse() != nullptr) ? "has_else" : "");
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
  visitChildren(node);
}

auto ASTPrinter::visitWhileStmt(const WhileStmt &node) -> void {
  auto fmtStr =
      fmt::format(FMT_COMPILE("{}{}WhileStmt{} {} {}"), formatPrefix(),
                  text_colors::MAGNETA, text_colors::RESET,
                  formatAddress(&node), formatRegion(node.getLocation()));
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
  visitChildren(node);
}

auto ASTPrinter::visitReturnStmt(const ReturnStmt &node) -> void {
  auto fmtStr =
      fmt::format(FMT_COMPILE("{}{}ReturnStmt{} {} {}"), formatPrefix(),
                  text_colors::MAGNETA, text_colors::RESET,
                  formatAddress(&node), formatRegion(node.getLocation()));
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
  visitChildren(node);
}

auto ASTPrinter::visitDeclStmt(const DeclStmt &node) -> void {
  auto fmtStr =
      fmt::format(FMT_COMPILE("{}{}DeclStmt{} {} {}"), formatPrefix(),
                  text_colors::MAGNETA, text_colors::RESET,
                  formatAddress(&node), formatRegion(node.getLocation()));
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
  visitChildren(node);
}

auto ASTPrinter::visitCallExpr(const CallExpr &node) -> void {
  auto fmtStr =
      fmt::format(FMT_COMPILE("{}{}CallExpr{} {} {}"), formatPrefix(),
                  text_colors::MAGNETA, text_colors::RESET,
                  formatAddress(&node), formatRegion(node.getLocation()));
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
  visitChildren(node);
}

auto ASTPrinter::visitParenExpr(const ParenExpr &node) -> void {
  auto fmtStr =
      fmt::format(FMT_COMPILE("{}{}ParenExpr{} {} {}"), formatPrefix(),
                  text_colors::MAGNETA, text_colors::RESET,
                  formatAddress(&node), formatRegion(node.getLocation()));
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
  visitChildren(node);
}

auto ASTPrinter::visitDeclRefExpr(const DeclRefExpr &node) -> void {
  auto fmtStr = fmt::format(
      FMT_COMPILE("{}{}DeclRefExpr{} {} {} {}"), formatPrefix(),
      text_colors::MAGNETA, text_colors::RESET, formatAddress(&node),
      formatRegionWithIdent(node.getLocation(), node.ident), node.getName());
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
}

auto ASTPrinter::visitArraySubscriptExpr(const ArraySubscriptExpr &node)
    -> void {
  auto fmtStr =
      fmt::format(FMT_COMPILE("{}{}ArraySubscriptExpr{} {} {}"), formatPrefix(),
                  text_colors::MAGNETA, text_colors::RESET,
                  formatAddress(&node), formatRegion(node.getLocation()));
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
  visitChildren(node);
}

auto ASTPrinter::visitImplicitCastExpr(const ImplicitCastExpr &node) -> void {
  auto fmtStr =
      fmt::format(FMT_COMPILE("{}{}ImplicitCastExpr{} {} {}"), formatPrefix(),
                  text_colors::MAGNETA, text_colors::RESET,
                  formatAddress(&node), formatRegion(node.getLocation()));
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
  visitChildren(node);
}

auto ASTPrinter::visitBinaryOperator(const BinaryOperator &node) -> void {
  auto fmtStr = fmt::format(
      FMT_COMPILE("{}{}BinaryOperator{} {} {} {}"), formatPrefix(),
      text_colors::MAGNETA, text_colors::RESET, formatAddress(&node),
      formatRegion(node.getLocation()), node.getOpToken().getLexeme());
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
  visitChildren(node);
}

auto ASTPrinter::visitUnaryOperator(const UnaryOperator &node) -> void {
  auto fmtStr = fmt::format(
      FMT_COMPILE("{}{}UnaryOperator{} {} {} {}"), formatPrefix(),
      text_colors::MAGNETA, text_colors::RESET, formatAddress(&node),
      formatRegion(node.getLocation()), node.getOpToken().getLexeme());
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
  visitChildren(node);
}

auto ASTPrinter::visitIntegerLiteral(const IntegerLiteral &node) -> void {
  auto fmtStr =
      fmt::format(FMT_COMPILE("{}{}IntegerLiteral{} {} {} {}{}{}"),
                  formatPrefix(), text_colors::MAGNETA, text_colors::RESET,
                  formatAddress(&node), formatRegion(node.getLocation()),
                  text_colors::CYAN, node.getLit(), text_colors::RESET);
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
}

auto ASTPrinter::visitFloatLiteral(const FloatLiteral &node) -> void {
  auto fmtStr =
      fmt::format(FMT_COMPILE("{}{}FloatLiteral{} {} {} {}{}{}"),
                  formatPrefix(), text_colors::MAGNETA, text_colors::RESET,
                  formatAddress(&node), formatRegion(node.getLocation()),
                  text_colors::CYAN, node.getLit(), text_colors::RESET);
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
}

auto ASTPrinter::visitBoolLiteral(const BoolLiteral &node) -> void {
  auto fmtStr = fmt::format(
      FMT_COMPILE("{}{}BoolLiteral{} {} {} {}{}{}"), formatPrefix(),
      text_colors::MAGNETA, text_colors::RESET, formatAddress(&node),
      formatRegion(node.getLocation()), text_colors::CYAN,
      ((node.getLit()) ? "true" : "false"), text_colors::RESET);
  fmt::println("{}", fmtStr);
  this->currFile = node.getLocation().fileName.value();
  this->currLine = node.getLocation().endLineNo;
  this->currCol = node.getLocation().endColumnNo;
}

} // namespace mccomp
