#include "ast/_internal/ast_node.h"

namespace mccomp {

ASTNode::ASTNode(NodeKind nodeKind, SourceLocation sourceLocation)
    : nodeKind(nodeKind), sourceLocation(std::move(sourceLocation)) {}

SourceLocation::SourceLocation(std::size_t lineNo, std::size_t startColumnNo,
                               std::size_t endColumnNo)
    : fileName(), lineNo(lineNo), startColumnNo(startColumnNo),
      endColumnNo(endColumnNo) {}

SourceLocation::SourceLocation(std::size_t lineNo, std::size_t startColumnNo,
                               std::size_t endColumnNo, std::string fileName)
    : fileName(std::move(fileName)), lineNo(lineNo),
      startColumnNo(startColumnNo), endColumnNo(endColumnNo) {}

auto SourceLocation::getView() const -> SourceLocation::View {
  return View{
      this->fileName,
      this->lineNo,
      this->startColumnNo,
      this->endColumnNo,
  };
}

} // namespace mccomp
