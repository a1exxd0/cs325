#pragma once

#include "error/error.h"
#include "tokens/tokens.h"
#include <ast/_internal/ast_node.h>
#include <ast/_internal/type.h>
#include <ast/_internal/visitor.h>

namespace mccomp {

class Type;

class Expr : public ASTNode {
  Type *type;

public:
  Expr(NodeKind kind, SourceLocation loc) : ASTNode(kind, std::move(loc)) {}
  auto getType() -> Type * { return type; }
  auto getType() const -> const Type * { return type; }
  auto setType(Type *type) -> void { this->type = type; }
};

class DeclRefExpr : public Expr {
  std::string name;
  Token ident;
  std::optional<ASTNode *> reference;

public:
  // oops im cheating
  friend class ASTPrinter;

  DeclRefExpr(Token ident, SourceLocation loc)
      : Expr(NK_DeclRefExpr, std::move(loc)), ident(std::move(ident)),
        reference() {
    this->name = this->ident.getLexeme();
  }

  auto getName() -> std::string & { return this->name; }
  auto getName() const -> std::string_view { return this->name; }

  auto getReference() -> std::optional<ASTNode *> { return this->reference; }
  auto getReference() const -> std::optional<const ASTNode *> {
    return this->reference;
  }
  auto setReference(ASTNode *node) -> void { this->reference = node; }

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
};

class ImplicitCastExpr : public Expr {
  ASTNode *expr;

public:
  ImplicitCastExpr(ASTNode *expr, SourceLocation loc)
      : Expr(NK_ImplicitCastExpr, std::move(loc)), expr(expr) {}

  auto getExpr() -> ASTNode * { return expr; }
  auto getExpr() const -> ASTNode * { return expr; }

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
};

class CallExpr final : public Expr {
  ImplicitCastExpr *callee;
  std::vector<Expr *> args;

public:
  CallExpr(ImplicitCastExpr *callee, std::vector<Expr *> args,
           SourceLocation loc)
      : Expr(NK_CallExpr, std::move(loc)), callee(callee),
        args(std::move(args)) {}

  auto getCallee() -> ImplicitCastExpr * { return callee; }
  auto getCallee() const -> const ImplicitCastExpr * { return callee; }

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
};

class ParenExpr : public Expr {
  ASTNode *inner;

public:
  ParenExpr(ASTNode *inner, SourceLocation loc)
      : Expr(NK_ParenExpr, std::move(loc)), inner(inner) {}

  auto getInner() -> ASTNode * { return inner; }
  auto getInner() const -> const ASTNode * { return inner; }

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
};

class ArraySubscriptExpr : public Expr {
  ASTNode *array;
  ASTNode *index;

public:
  ArraySubscriptExpr(ASTNode *array, ASTNode *index, SourceLocation loc)
      : Expr(NK_ArraySubscriptExpr, std::move(loc)), array(array),
        index(index) {}

  auto getArray() -> ASTNode * { return array; }
  auto getArray() const -> const ASTNode * { return array; }

  auto getIndex() -> ASTNode * { return index; }
  auto getIndex() const -> ASTNode * { return index; }

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
  ASTNode *lhs;
  ASTNode *rhs;

public:
  BinaryOperator(Token opToken, ASTNode *lhs, ASTNode *rhs, SourceLocation loc)
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

  auto getLHS() -> ASTNode * { return lhs; }
  auto getLHS() const -> const ASTNode * { return lhs; }

  auto getRHS() -> ASTNode * { return rhs; }
  auto getRHS() const -> const ASTNode * { return rhs; }

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
  ASTNode *expr;

public:
  UnaryOperator(Token opToken, ASTNode *expr, SourceLocation loc)
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

  auto getExpr() -> ASTNode * { return expr; }
  auto getExpr() const -> const ASTNode * { return expr; }

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
};

class IntegerLiteral : public Expr {
  int lit;

public:
  IntegerLiteral(int lit, SourceLocation loc)
      : Expr(NK_IntegerLiteral, std::move(loc)), lit(lit) {}

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
};

class FloatLiteral : public Expr {
  double lit;

public:
  FloatLiteral(double lit, SourceLocation loc)
      : Expr(NK_FloatLiteral, std::move(loc)), lit(lit) {}

  auto getLit() const -> int { return lit; }

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
};
class BoolLiteral : public Expr {
  bool lit;

public:
  BoolLiteral(bool lit, SourceLocation loc)
      : Expr(NK_FloatLiteral, std::move(loc)), lit(lit) {}

  auto getLit() const -> int { return lit; }

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
};

} // namespace mccomp
