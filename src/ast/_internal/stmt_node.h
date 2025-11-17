#pragma once

#include <ast/_internal/ast_node.h>
#include <ast/_internal/type.h>
#include <ast/_internal/visitor.h>

namespace mccomp {
class Stmt : public ASTNode {
public:
  Stmt(NodeKind kind, SourceLocation loc) : ASTNode(kind, std::move(loc)) {}
};

class CompoundStmt final : public Stmt {
  std::vector<ASTNode *> stmts;

public:
  CompoundStmt(std::vector<ASTNode *> stmts, SourceLocation loc)
      : Stmt(NK_CompoundStmt, std::move(loc)), stmts(std::move(stmts)) {}

  auto getStmts() -> std::vector<ASTNode *> & { return stmts; }
  auto getStmts() const -> const std::vector<ASTNode *> & { return stmts; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitCompoundStmt(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitCompoundStmt(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override { return stmts; }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return std::vector<const ASTNode *>(stmts.begin(), stmts.end());
  }
};

class IfStmt final : public Stmt {
  ASTNode *cond;
  ASTNode *thenStmt;
  ASTNode *elseStmt; // nullable

public:
  IfStmt(ASTNode *cond, ASTNode *thenStmt, ASTNode *elseStmt,
         SourceLocation loc)
      : Stmt(NK_IfStmt, std::move(loc)), cond(cond), thenStmt(thenStmt),
        elseStmt(elseStmt) {}

  auto getCond() -> ASTNode * { return cond; }
  auto getCond() const -> const ASTNode * { return cond; }

  auto getThen() -> ASTNode * { return thenStmt; }
  auto getThen() const -> const ASTNode * { return thenStmt; }

  auto getElse() -> ASTNode * { return elseStmt; }
  auto getElse() const -> const ASTNode * { return elseStmt; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitIfStmt(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitIfStmt(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override {
    return elseStmt ? std::vector<ASTNode *>{cond, thenStmt, elseStmt}
                    : std::vector<ASTNode *>{cond, thenStmt};
  }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return elseStmt ? std::vector<const ASTNode *>{cond, thenStmt, elseStmt}
                    : std::vector<const ASTNode *>{cond, thenStmt};
  }
};

class WhileStmt final : public Stmt {
  ASTNode *cond;
  ASTNode *body;

public:
  WhileStmt(ASTNode *cond, ASTNode *body, SourceLocation loc)
      : Stmt(NK_WhileStmt, std::move(loc)), cond(cond), body(body) {}

  auto getCond() -> ASTNode * { return cond; }
  auto getCond() const -> const ASTNode * { return cond; }

  auto getBody() -> ASTNode * { return body; }
  auto getBody() const -> const ASTNode * { return body; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitWhileStmt(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitWhileStmt(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override { return {cond, body}; }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return {cond, body};
  }
};

class ReturnStmt final : public Stmt {
  ASTNode *expr; // nullable for void returns

public:
  ReturnStmt(ASTNode *expr, SourceLocation loc)
      : Stmt(NK_ReturnStmt, std::move(loc)), expr(expr) {}

  auto getExpr() -> ASTNode * { return expr; }
  auto getExpr() const -> const ASTNode * { return expr; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitReturnStmt(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitReturnStmt(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override {
    return expr ? std::vector<ASTNode *>{expr} : std::vector<ASTNode *>{};
  }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return expr ? std::vector<const ASTNode *>{expr}
                : std::vector<const ASTNode *>{};
  }
};

class DeclStmt final : public Stmt {
  ASTNode *decl;

public:
  DeclStmt(ASTNode *decl, SourceLocation loc)
      : Stmt(NK_DeclStmt, std::move(loc)), decl(decl) {}

  auto getDecl() -> ASTNode * { return decl; }
  auto getDecl() const -> const ASTNode * { return decl; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitDeclStmt(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitDeclStmt(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override { return {decl}; }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return {decl};
  }
};

} // namespace mccomp
