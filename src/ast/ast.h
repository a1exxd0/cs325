#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <ostream>
#include <string>
#include <tokens/tokens.h>

namespace mccomp {

/// ASTnode - Base class for all AST nodes.
class ASTNode {

public:
  virtual ~ASTNode() {}
  virtual llvm::Value *codegen() = 0;
  virtual auto to_string() const -> std::string;
  friend auto operator<<(std::ostream &os, const ASTNode &node)
      -> std::ostream &;
};

auto operator<<(std::ostream &os, const ASTNode &node) -> std::ostream &;

} // namespace mccomp
