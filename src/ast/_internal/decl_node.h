#pragma once

#include <ast/_internal/ast_node.h>
#include <ast/_internal/ctx.h>
#include <ast/_internal/type.h>
#include <ast/_internal/visitor.h>

namespace mccomp {

class Decl : public ASTNode {
  Type *type;
  std::string name;
  Token ident;
  bool valid = true;
  bool used = false;

public:
  Decl(NodeKind kind, Token ident, Type *type, SourceLocation loc)
      : ASTNode(kind, std::move(loc)), type(type), ident(std::move(ident)) {
    this->name = this->ident.asIdent();
  }

  auto getType() -> Type * { return type; }
  auto getType() const -> const Type * { return type; }
  auto setType(Type *type) -> void { this->type = type; }

  auto invalidate() -> void { this->valid = false; }
  auto isValid() const -> bool { return this->valid; }

  auto markUsed() -> void { this->used = true; }
  auto isUsed() const -> bool { return this->used; }

  auto getName() -> std::string & { return name; }
  auto getName() const -> std::string_view { return name; }

  auto getIdent() -> Token & { return ident; }
  auto getIdent() const -> const Token & { return ident; }
};

class ParmVarDecl final : public Decl {
public:
  ParmVarDecl(Token ident, Type *type, SourceLocation loc)
      : Decl(NK_ParmVarDecl, std::move(ident), type, std::move(loc)) {}

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

  static auto classof(const ASTNode *n) -> bool {
    return n->getKind() == NK_ParmVarDecl;
  }
};

class VarDecl final : public Decl {
  ASTNode *init = nullptr; // nullable

public:
  VarDecl(Token ident, Type *type, ASTNode *init, SourceLocation loc)
      : Decl(NK_VarDecl, std::move(ident), type, std::move(loc)), init(init) {}

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

  static auto classof(const ASTNode *n) -> bool {
    return n->getKind() == NK_VarDecl;
  }
};

class FunctionDecl final : public Decl {
  std::vector<ParmVarDecl *> params;
  ASTNode *body = nullptr; // nullable for extern
  FunctionType *functionType;

public:
  FunctionDecl(Token ident, Type *returnType, std::vector<ParmVarDecl *> params,
               ASTNode *body, ASTContext &ctx, SourceLocation loc)
      : Decl(NK_FunctionDecl, std::move(ident), returnType, std::move(loc)),
        params(std::move(params)), body(body) {
    auto paramsAsTypes = std::vector<Type *>();
    paramsAsTypes.reserve(this->params.size());
    for (auto &param : this->params)
      paramsAsTypes.push_back(param->getType());
    auto key = FunctionType(returnType, paramsAsTypes);
    auto functionType = ctx.getFunctionType(returnType, paramsAsTypes);
    this->functionType = dynamic_cast<FunctionType *>(functionType);
  }

  auto getFunctionType() -> FunctionType * { return functionType; }
  auto getFunctionType() const -> const FunctionType * { return functionType; }

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
    for (auto *p : params)
      out.push_back(p);
    if (body)
      out.push_back(body);
    return out;
  }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    auto out = std::vector<const ASTNode *>();
    out.reserve(params.size() + (body ? 1 : 0));
    for (auto *p : params)
      out.push_back(p);
    if (body)
      out.push_back(body);
    return out;
  }

  static auto classof(const ASTNode *n) -> bool {
    return n->getKind() == NK_FunctionDecl;
  }
};

} // namespace mccomp
