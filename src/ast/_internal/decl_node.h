#pragma once

#include <ast/_internal/ast_node.h>
#include <ast/_internal/type.h>
#include <ast/_internal/visitor.h>

namespace mccomp {

class Decl : public ASTNode {
  Type *type;
  std::string name;

public:
  Decl(NodeKind kind, std::string name, Type *type, SourceLocation loc)
      : ASTNode(kind, std::move(loc)), type(type), name(std::move(name)) {}

  auto getType() -> Type * { return type; }
  auto getType() const -> const Type * { return type; }
  auto setType(Type *type) -> void { this->type = type; }

  auto getName() -> std::string & { return name; }
  auto getName() const -> std::string_view { return name; }
};

class ParmVarDecl final : public Decl {
public:
  ParmVarDecl(std::string name, Type *type, SourceLocation loc)
      : Decl(NK_ParmVarDecl, std::move(name), type, std::move(loc)) {}

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitParmVarDecl(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitParmVarDecl(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override { return {}; }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return {};
  }
};

class VarDecl final : public Decl {
  ASTNode *init; // nullable

public:
  VarDecl(std::string name, Type *type, ASTNode *init, SourceLocation loc)
      : Decl(NK_VarDecl, std::move(name), type, std::move(loc)), init(init) {}

  auto getInit() -> ASTNode * { return init; }
  auto getInit() const -> const ASTNode * { return init; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitVarDecl(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitVarDecl(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override {
    return init ? std::vector<ASTNode *>{init} : std::vector<ASTNode *>{};
  }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    return init ? std::vector<const ASTNode *>{init}
                : std::vector<const ASTNode *>{};
  }
};

class FunctionDecl final : public Decl {
  std::vector<ParmVarDecl *> params;
  ASTNode *body; // nullable for extern

public:
  FunctionDecl(std::string name, Type *type, std::vector<ParmVarDecl *> params,
               ASTNode *body, SourceLocation loc)
      : Decl(NK_FunctionDecl, std::move(name), type, std::move(loc)),
        params(std::move(params)), body(body) {}

  auto getParams() -> std::vector<ParmVarDecl *> & { return params; }
  auto getParams() const -> const std::vector<ParmVarDecl *> & {
    return params;
  }

  auto getBody() -> ASTNode * { return body; }
  auto getBody() const -> const ASTNode * { return body; }

  auto isExtern() const -> bool { return body == nullptr; }

  auto accept(ASTVisitor &visitor) -> void override {
    visitor.visitFunctionDecl(*this);
  }

  auto accept(ConstASTVisitor &visitor) const -> void override {
    visitor.visitFunctionDecl(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override {
    auto out = std::vector<ASTNode *>();
    out.reserve(params.size() + (body ? 1 : 0));
    out.insert(out.end(), params.begin(), params.end());
    if (body)
      out.push_back(body);
    return out;
  }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    auto out = std::vector<const ASTNode *>();
    out.reserve(params.size() + (body ? 1 : 0));
    out.insert(out.end(), params.begin(), params.end());
    if (body)
      out.push_back(body);
    return out;
  }
};

} // namespace mccomp
