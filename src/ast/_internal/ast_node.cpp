#include <ast/_internal/ast_node.h>

namespace mccomp {
auto to_string(ASTNode::NodeKind kind) -> std::string {
  using K = ASTNode::NodeKind;
  switch (kind) {
  case K::NK_TranslationUnit:
    return "TranslationUnit";
  case K::NK_ExternDecl:
    return "ExternDecl";
  case K::NK_FunctionDecl:
    return "FunctionDecl";
  case K::NK_VarDecl:
    return "VarDecl";
  case K::NK_ParmVarDecl:
    return "ParmVarDecl";

  case K::NK_CompoundStmt:
    return "CompoundStmt";
  case K::NK_IfStmt:
    return "IfStmt";
  case K::NK_WhileStmt:
    return "WhileStmt";
  case K::NK_ReturnStmt:
    return "ReturnStmt";
  case K::NK_DeclStmt:
    return "DeclStmt";

  case K::NK_CallExpr:
    return "CallExpr";
  case K::NK_ParenExpr:
    return "ParenExpr";
  case K::NK_DeclRefExpr:
    return "DeclRefExpr";
  case K::NK_ArraySubscriptExpr:
    return "ArraySubscriptExpr";
  case K::NK_ImplicitCastExpr:
    return "ImplicitCastExpr";

  case K::NK_BinaryOperator:
    return "BinaryOperator";
  case K::NK_UnaryOperator:
    return "UnaryOperator";

  case K::NK_IntegerLiteral:
    return "IntegerLiteral";
  case K::NK_FloatLiteral:
    return "FloatLiteral";
  case K::NK_BoolLiteral:
    return "BoolLiteral";
  }
  return "Unknown";
}

} // namespace mccomp
