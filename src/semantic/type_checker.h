#pragma once

#include <ast/ast.h>

namespace mccomp {

class TypeChecker final : public ASTVisitor {
  ASTContext &ctx;
  bool success = true;
  std::optional<std::pair<Type *, std::string>> scopeReturnType;

  auto convertNodeToNumeral(Expr *expr) -> tl::expected<Expr *, ClangError>;

public:
  struct Symbol {
    enum SymbolType {
      Func,
      Var,
    };

    SymbolType symbolType;
    std::string name;
    ASTNode *decl;
    Type *type;

    Symbol(SymbolType symbolType, std::string_view name, ASTNode *decl,
           Type *type)
        : symbolType(symbolType), name(name), decl(decl), type(type) {}

    auto to_string() const -> std::string {
      return fmt::format("{}: ({}:'{}':{})", name,
                         (symbolType == Func ? "func" : "var"),
                         type->to_string(), static_cast<void *>(decl));
    }
  };

  struct Frame {
    std::unordered_map<std::string, Symbol> functions;
    std::unordered_map<std::string, Symbol> variables;

    auto to_string() const -> std::string {
      auto ss = std::stringstream();
      ss << "Functions: \n";
      for (const auto &[_, v] : functions) {
        ss << v.to_string() << "\n";
      }
      ss << "Variables: \n";
      for (const auto &[_, v] : variables) {
        ss << v.to_string() << "\n";
      }
      return ss.str();
    }
  };

  class SymbolTable {
    std::vector<Frame> table;

  public:
    SymbolTable() : table() {
      table.reserve(10);
      table.push_back(Frame());
    }

    auto to_string() const -> std::string {
      auto ss = std::stringstream();
      for (auto i = std::size_t{}; i < table.size(); ++i) {
        ss << "<---------- FRAME " << i << " ---------->\n";
        ss << table[i].to_string();
      }

      return ss.str();
    }

    auto find(const std::string &symbolName) const
        -> std::optional<std::reference_wrapper<const Symbol>> {
      for (auto it = table.rbegin(); it != table.rend(); it++) {
        auto symbolVar = it->variables.find(symbolName);
        if (symbolVar != it->variables.end())
          return std::ref(symbolVar->second);
        auto symbolFunc = it->functions.find(symbolName);
        if (symbolFunc != it->functions.end())
          return std::ref(symbolFunc->second);
      }

      return std::nullopt;
    }

    auto addSymbol(Symbol symbol)
        -> tl::expected<std::reference_wrapper<const Symbol>,
                        std::reference_wrapper<const Symbol>> {
      auto &insertInto = (symbol.symbolType == Symbol::Func)
                             ? table.back().functions
                             : table.back().variables;
      auto &checkOther = (symbol.symbolType == Symbol::Func)
                             ? table.back().variables
                             : table.back().functions;

      auto existsInOther = checkOther.find(symbol.name);
      if (existsInOther != checkOther.end())
        return tl::unexpected(existsInOther->second);

      auto [it, success] = insertInto.insert({symbol.name, symbol});
      if (success) {
        return std::ref(it->second);
      } else {
        return tl::unexpected(std::ref(it->second));
      }
    }

    auto pushFrame() -> void {
      if (bufferedFrame) {
        table.push_back(std::move(bufferedFrame.value()));
        bufferedFrame = std::nullopt;
      } else {
        table.push_back(Frame());
      }
    }

    auto bufferFrame(Frame frame) -> void {
      assert(!this->bufferedFrame);
      bufferedFrame = frame;
    }

    auto popFrame() -> std::optional<Frame> {
      if (table.empty())
        return std::nullopt;

      auto lastFrame = std::move(table.back());
      table.pop_back();

      return lastFrame;
    }

  private:
    std::optional<Frame> bufferedFrame;
  };

  TypeChecker(ASTContext &ctx) : ASTVisitor(), ctx(ctx) {}
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

  auto isSuccess() const -> bool { return success; }

private:
  SymbolTable symbolTable;

  auto decayFunction(Expr *expr) -> Expr *;
  auto decayArray(Expr *expr) -> Expr *;
  auto promoteInt(Expr *expr) -> Expr *;
  auto promoteBoolToFloat(Expr *expr) -> Expr *;
  auto promoteBoolToInt(Expr *expr) -> Expr *;
};

} // namespace mccomp
