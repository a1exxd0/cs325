#pragma once

#include "ast/_internal/expr_node.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include <ast/ast.h>
#include <fmt/compile.h>
#include <fmt/format.h>

namespace mccomp {

class CodeGen final : public ConstASTVisitor {
  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::Module> module_;
  std::unique_ptr<llvm::IRBuilder<>> builder_;

  std::unordered_map<const Decl *, llvm::Value *> named_values_;
  std::unordered_map<const Decl *, llvm::GlobalVariable *> globals_;
  llvm::Function *current_function_{nullptr};
  llvm::Value *last_value_{nullptr};

public:
  CodeGen(std::string_view moduleName)
      : context_(std::make_unique<llvm::LLVMContext>()),
        module_(std::make_unique<llvm::Module>(moduleName, *context_)),
        builder_(std::make_unique<llvm::IRBuilder<>>(*context_)),
        named_values_(), globals_() {};
  auto visitTranslationUnit(const TranslationUnit &node) -> void override;
  auto visitFunctionDecl(const FunctionDecl &node) -> void override;
  auto visitVarDecl(const VarDecl &node) -> void override;
  auto visitParmVarDecl(const ParmVarDecl &node) -> void override;
  auto visitCompoundStmt(const CompoundStmt &node) -> void override;
  auto visitIfStmt(const IfStmt &node) -> void override;
  auto visitWhileStmt(const WhileStmt &node) -> void override;
  auto visitReturnStmt(const ReturnStmt &node) -> void override;
  auto visitDeclStmt(const DeclStmt &node) -> void override;
  auto visitCallExpr(const CallExpr &node) -> void override;
  auto visitParenExpr(const ParenExpr &node) -> void override;
  auto visitDeclRefExpr(const DeclRefExpr &node) -> void override;
  auto visitArraySubscriptExpr(const ArraySubscriptExpr &node) -> void override;
  auto visitImplicitCastExpr(const ImplicitCastExpr &node) -> void override;
  auto visitBinaryOperator(const BinaryOperator &node) -> void override;
  auto visitUnaryOperator(const UnaryOperator &node) -> void override;
  auto visitIntegerLiteral(const IntegerLiteral &node) -> void override;
  auto visitFloatLiteral(const FloatLiteral &node) -> void override;
  auto visitBoolLiteral(const BoolLiteral &node) -> void override;
  auto print(llvm::raw_fd_ostream &f) -> void { module_->print(f, nullptr); }

private:
  auto convertType(const Type *type) -> llvm::Type *;
  auto convertToBoolean(llvm::Value *value) -> llvm::Value *;

  auto createArithmeticOp(BinaryOperator::Operator op, llvm::Value *lhs,
                          llvm::Value *rhs) -> llvm::Value *;
  auto createComparisonOp(BinaryOperator::Operator op, llvm::Value *lhs,
                          llvm::Value *rhs) -> llvm::Value *;
  auto createLogicalOp(BinaryOperator::Operator op, const Expr *lhs_expr,
                       const Expr *rhs_expr) -> llvm::Value *;
  auto handleAssignment(const Expr *lhs_expr, const Expr *rhs_value)
      -> llvm::Value *;

  std::unordered_map<Type *, llvm::PointerType *> elementToLLVMPtr;
};

} // namespace mccomp
