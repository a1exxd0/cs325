#pragma once

#include "error/error.h"
#include "tokens/tokens.h"
#include <ast/_internal/ast_node.h>
#include <ast/_internal/ctx.h>
#include <ast/_internal/type.h>
#include <ast/_internal/visitor.h>

namespace mccomp {

class Type;

class Expr : public ASTNode {
  Type *type = nullptr;
  bool valid = true;

public:
  enum ValueCategory {
    LValue,
    RValue,
  };

  Expr(NodeKind kind, SourceLocation loc) : ASTNode(kind, std::move(loc)) {}

  auto getType() -> Type * { return type; }
  auto getType() const -> const Type * { return type; }
  auto setType(Type *type) -> void { this->type = type; }

  auto getValueCategory() const -> std::optional<ValueCategory> {
    return valueCategory;
  }
  auto setValueCategory(ValueCategory cat) -> void { valueCategory = cat; }

  auto invalidate() -> void { valid = false; }
  auto isValid() const -> bool { return valid; }

private:
  std::optional<ValueCategory> valueCategory = std::nullopt;
};

class DeclRefExpr : public Expr {
  std::string name;
  Token ident;
  std::optional<ASTNode *> reference;

public:
  enum RefType {
    FUNCTION,
    VARIABLE,
  };

  // oops im cheating
  friend class ASTPrinter;
  friend class TypeChecker;

  DeclRefExpr(Token ident, RefType refType, SourceLocation loc)
      : Expr(NK_DeclRefExpr, std::move(loc)), ident(std::move(ident)),
        reference(), refType(refType) {
    this->name = this->ident.getLexeme();
  }

  auto getName() -> std::string & { return this->name; }
  auto getName() const -> std::string_view { return this->name; }

  auto getReference() -> std::optional<ASTNode *> { return this->reference; }
  auto getReference() const -> std::optional<const ASTNode *> {
    return this->reference;
  }
  auto setReference(ASTNode *node) -> void { this->reference = node; }

  auto getRefType() const -> RefType { return refType; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitDeclRefExpr(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitDeclRefExpr(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override { return {}; }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return {};
  }

  static auto classof(const ASTNode *n) -> bool {
    return n->getKind() == NK_DeclRefExpr;
  }

private:
  RefType refType;
};

class ImplicitCastExpr : public Expr {
private:
  ASTNode *expr;
  CastType castType = static_cast<CastType>(-1);

public:
  ImplicitCastExpr(ASTNode *expr, SourceLocation loc)
      : Expr(NK_ImplicitCastExpr, std::move(loc)), expr(expr) {}

  auto getExpr() -> ASTNode * { return expr; }
  auto getExpr() const -> ASTNode * { return expr; }

  auto getCastType() const -> CastType { return this->castType; }
  auto setCastType(CastType castType) -> void { this->castType = castType; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitImplicitCastExpr(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitImplicitCastExpr(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override { return {expr}; }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return {expr};
  }

  static auto classof(const ASTNode *n) -> bool {
    return n->getKind() == NK_ImplicitCastExpr;
  }
};

class CallExpr final : public Expr {
  Expr *callee;
  std::vector<Expr *> args;

public:
  CallExpr(Expr *callee, std::vector<Expr *> args, SourceLocation loc)
      : Expr(NK_CallExpr, std::move(loc)), callee(callee),
        args(std::move(args)) {}

  auto getCallee() -> Expr * { return callee; }
  auto getCallee() const -> const Expr * { return callee; }
  auto setCallee(Expr *expr) -> void { this->callee = expr; }

  auto getArgs() -> std::vector<Expr *> & { return args; }
  auto getArgs() const -> std::vector<const Expr *> {
    auto vec = std::vector<const Expr *>(args.begin(), args.end());
    return vec;
  }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitCallExpr(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitCallExpr(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override {
    auto out = std::vector<ASTNode *>();
    out.reserve(args.size() + 1);
    out.push_back(callee);
    out.insert(out.end(), args.begin(), args.end());
    return out;
  }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    auto out = std::vector<const ASTNode *>();
    out.reserve(args.size() + 1);
    out.push_back(callee);
    out.insert(out.end(), args.begin(), args.end());
    return out;
  }

  static auto classof(const ASTNode *n) -> bool {
    return n->getKind() == NK_CallExpr;
  }
};

class ParenExpr : public Expr {
  Expr *inner;

public:
  ParenExpr(Expr *inner, SourceLocation loc)
      : Expr(NK_ParenExpr, std::move(loc)), inner(inner) {}

  auto getInner() -> Expr * { return inner; }
  auto getInner() const -> const Expr * { return inner; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitParenExpr(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitParenExpr(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override { return {inner}; }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return {inner};
  }

  static auto classof(const ASTNode *n) -> bool {
    return n->getKind() == NK_ParenExpr;
  }
};

class ArraySubscriptExpr : public Expr {
  Expr *array;
  Expr *index;

public:
  ArraySubscriptExpr(Expr *array, Expr *index, SourceLocation loc)
      : Expr(NK_ArraySubscriptExpr, std::move(loc)), array(array),
        index(index) {}

  auto getArray() -> Expr * { return array; }
  auto getArray() const -> const Expr * { return array; }
  auto setArray(Expr *expr) -> void { this->array = expr; }

  auto getIndex() -> Expr * { return index; }
  auto getIndex() const -> Expr * { return index; }
  auto setIndex(Expr *expr) -> void { this->index = expr; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitArraySubscriptExpr(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitArraySubscriptExpr(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override {
    return {array, index};
  }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return {array, index};
  }

  static auto classof(const ASTNode *n) -> bool {
    return n->getKind() == NK_ArraySubscriptExpr;
  }
};

class BinaryOperator : public Expr {
public:
  enum Operator {
    OP_ASSIGN,
    OP_OR,
    OP_AND,
    OP_EQ,
    OP_NEQ,
    OP_LEQ,
    OP_LT,
    OP_GEQ,
    OP_GT,
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_MOD,
  };

private:
  Token opToken;
  Operator op;
  Expr *lhs;
  Expr *rhs;

public:
  BinaryOperator(Token opToken, Expr *lhs, Expr *rhs, SourceLocation loc)
      : Expr(NK_BinaryOperator, std::move(loc)), opToken(std::move(opToken)),
        lhs(lhs), rhs(rhs) {
    auto op = [](const Token &tok) -> Operator {
      switch (tok.getTokenType()) {
      case TokenType::ASSIGN:
        return OP_ASSIGN;
      case TokenType::OR:
        return OP_OR;
      case TokenType::AND:
        return OP_AND;
      case TokenType::EQ:
        return OP_EQ;
      case TokenType::NE:
        return OP_NEQ;
      case TokenType::LE:
        return OP_LEQ;
      case TokenType::LT:
        return OP_LT;
      case TokenType::GE:
        return OP_GEQ;
      case TokenType::GT:
        return OP_GT;
      case TokenType::PLUS:
        return OP_ADD;
      case TokenType::MINUS:
        return OP_SUB;
      case TokenType::ASTERIX:
        return OP_MUL;
      case TokenType::DIV:
        return OP_DIV;
      case TokenType::MOD:
        return OP_MOD;
      default:
        auto error = ClangError(
            ClangErrorSeverity::ERROR,
            fmt::format("failed to convert token type to binary operator with "
                        "token {}. Something went wrong in parsing.",
                        tok.getLexeme()));
        fmt::println(stderr, "{}", error.to_string());
        exit(2);
      }
    }(opToken);

    this->op = op;
  }

  auto getOpToken() const -> const Token & { return opToken; }

  auto getOp() const -> Operator { return op; }

  auto getLHS() -> Expr * { return lhs; }
  auto getLHS() const -> const Expr * { return lhs; }
  auto setLHS(Expr *expr) -> void { this->lhs = expr; }

  auto getRHS() -> Expr * { return rhs; }
  auto getRHS() const -> const Expr * { return rhs; }
  auto setRHS(Expr *expr) -> void { this->rhs = expr; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitBinaryOperator(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitBinaryOperator(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override { return {lhs, rhs}; }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return {lhs, rhs};
  }

  static auto classof(const ASTNode *n) -> bool {
    return n->getKind() == NK_BinaryOperator;
  }
};

class UnaryOperator : public Expr {
public:
  enum Operator {
    OP_NEG,
    OP_NOT,
  };

private:
  Token opToken;
  Operator op;
  Expr *expr;

public:
  UnaryOperator(Token opToken, Expr *expr, SourceLocation loc)
      : Expr(NK_UnaryOperator, std::move(loc)), opToken(std::move(opToken)),
        expr(expr) {
    auto op = [](const Token &tok) -> Operator {
      switch (tok.getTokenType()) {
      case TokenType::MINUS:
        return OP_NEG;
      case TokenType::NOT:
        return OP_NOT;
      default:
        auto error = ClangError(
            ClangErrorSeverity::ERROR,
            fmt::format("failed to convert token type to binary operator with "
                        "token {}. Something went wrong in parsing.",
                        tok.getLexeme()));
        fmt::println(stderr, "{}", error.to_string());
        exit(2);
      }
    }(opToken);

    this->op = op;
  }

  auto getOpToken() const -> const Token & { return opToken; }

  auto getOp() const -> Operator { return op; }

  auto getExpr() -> Expr * { return expr; }
  auto getExpr() const -> const Expr * { return expr; }
  auto setExpr(Expr *expr) -> void { this->expr = expr; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitUnaryOperator(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitUnaryOperator(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override { return {expr}; }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return {expr};
  }

  static auto classof(const ASTNode *n) -> bool {
    return n->getKind() == NK_UnaryOperator;
  }
};

class IntegerLiteral : public Expr {
  int lit;

public:
  IntegerLiteral(int lit, ASTContext &ctx, SourceLocation loc)
      : Expr(NK_IntegerLiteral, std::move(loc)), lit(lit) {
    this->setType(ctx.getIntType());
  }

  auto getLit() const -> int { return lit; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitIntegerLiteral(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitIntegerLiteral(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override { return {}; }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return {};
  }

  static auto classof(const ASTNode *n) -> bool {
    return n->getKind() == NK_IntegerLiteral;
  }
};

class FloatLiteral : public Expr {
  double lit;

public:
  FloatLiteral(double lit, ASTContext &ctx, SourceLocation loc)
      : Expr(NK_FloatLiteral, std::move(loc)), lit(lit) {
    this->setType(ctx.getFloatType());
  }

  auto getLit() const -> double { return lit; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitFloatLiteral(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitFloatLiteral(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override { return {}; }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return {};
  }

  static auto classof(const ASTNode *n) -> bool {
    return n->getKind() == NK_FloatLiteral;
  }
};
class BoolLiteral : public Expr {
  bool lit;

public:
  BoolLiteral(bool lit, ASTContext &ctx, SourceLocation loc)
      : Expr(NK_FloatLiteral, std::move(loc)), lit(lit) {
    this->setType(ctx.getBoolType());
  }

  auto getLit() const -> bool { return lit; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitBoolLiteral(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitBoolLiteral(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override { return {}; }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return {};
  }

  static auto classof(const ASTNode *n) -> bool {
    return n->getKind() == NK_BoolLiteral;
  }
};

} // namespace mccomp
