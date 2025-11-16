#include <ast/ast.h>
#include <ostream>

namespace mccomp {

auto operator<<(std::ostream &os, const ASTNode &node) -> std::ostream & {
  return os << node.to_string();
}

} // namespace mccomp
