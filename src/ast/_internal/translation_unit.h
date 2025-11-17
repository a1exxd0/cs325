#pragma once

#include <ast/_internal/ast_node.h>
#include <ast/_internal/visitor.h>

namespace mccomp {

class TranslationUnit final : public ASTNode {
  std::vector<ASTNode *> externs;
  std::vector<ASTNode *> decls;

public:
  TranslationUnit(std::vector<ASTNode *> externs, std::vector<ASTNode *> decls,
                  SourceLocation loc)
      : ASTNode(NK_TranslationUnit, std::move(loc)),
        externs(std::move(externs)), decls(std::move(decls)) {}

  auto getExterns() -> std::vector<ASTNode *> & { return externs; }
  auto getExterns() const -> const std::vector<ASTNode *> & { return externs; }

  auto getDecls() -> std::vector<ASTNode *> & { return decls; }
  auto getDecls() const -> const std::vector<ASTNode *> & { return decls; }

  void accept(ASTVisitor &visitor) override {
    visitor.visitTranslationUnit(*this);
  }

  void accept(ConstASTVisitor &visitor) const override {
    visitor.visitTranslationUnit(*this);
  }

  auto getChildren() -> std::vector<ASTNode *> override {
    auto out = std::vector<ASTNode *>();
    out.reserve(externs.size() + decls.size());
    out.insert(out.end(), externs.begin(), externs.end());
    out.insert(out.end(), decls.begin(), decls.end());
    return out;
  }

  auto getChildren() const -> std::vector<const ASTNode *> override {
    auto out = std::vector<const ASTNode *>();
    out.reserve(externs.size() + decls.size());
    out.insert(out.end(), externs.begin(), externs.end());
    out.insert(out.end(), decls.begin(), decls.end());
    return out;
  }
};

} // namespace mccomp
