#pragma once

#include <ast/_internal/ast_node.h>

namespace mccomp {

class CallExpr : ASTNode {};
class ParenExpr : ASTNode {};
class DeclRefExpr : ASTNode {};
class ArraySubscriptExpr : ASTNode {};
class ImplicitCastExpr : ASTNode {};
class BinaryOperator : ASTNode {};
class UnaryOperator : ASTNode {};
class IntegerLiteral : ASTNode {};
class FloatLiteral : ASTNode {};
class BoolLiteral : ASTNode {};

} // namespace mccomp
