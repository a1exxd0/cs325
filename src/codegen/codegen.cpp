#include "llvm/IR/Constants.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include <codegen/codegen.h>

namespace mccomp {

auto CodeGen::visitTranslationUnit(const TranslationUnit &node) -> void {
  for (const auto &ext : node.getExterns()) {
    ext->accept(*this);
  }

  for (const auto &decl : node.getDecls()) {
    decl->accept(*this);
  }

  if (llvm::verifyModule(*module_, &llvm::errs())) {
    llvm::errs() << "module verification failed, invalid\n";
  }
}

auto CodeGen::visitFunctionDecl(const FunctionDecl &node) -> void {
  auto paramTypes = std::vector<llvm::Type *>();
  paramTypes.reserve(node.getParams().size());
  for (const auto *param : node.getParams()) {
    paramTypes.push_back(convertType(param->getType()));
  }

  auto *return_type = convertType(node.getType());
  auto *func_type = llvm::FunctionType::get(return_type, paramTypes, false);

  auto *func = module_->getFunction(node.getName());
  if (!func) {
    func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage,
                                  node.getName(), module_.get());
  }

  if (node.isExtern()) {
    return;
  }

  current_function_ = func;

  auto *entry_bb = llvm::BasicBlock::Create(*context_, "entry", func);
  builder_->SetInsertPoint(entry_bb);

  auto idx = std::size_t();
  for (auto &arg : func->args()) {
    const auto *param = node.getParams()[idx];
    arg.setName(param->getName());

    auto *alloca =
        builder_->CreateAlloca(arg.getType(), nullptr, param->getName());
    builder_->CreateStore(&arg, alloca);

    named_values_[param] = alloca;
    ++idx;
  }

  node.getBody()->accept(*this);

  if (!builder_->GetInsertBlock()->getTerminator()) {
    if (return_type->isVoidTy()) {
      builder_->CreateRetVoid();
    } else {
      llvm::Value *zero_val = llvm::Constant::getNullValue(return_type);
      builder_->CreateRet(zero_val);
    }
  }

  if (llvm::verifyFunction(*func, &llvm::errs())) {
    llvm::errs() << "error: function verification failed for " << node.getName()
                 << "\n";
    func->print(llvm::errs());
  }

  current_function_ = nullptr;
  named_values_.clear();
}

auto CodeGen::visitVarDecl(const VarDecl &node) -> void {
  if (current_function_) {
    auto *alloca = builder_->CreateAlloca(convertType(node.getType()), nullptr,
                                          node.getName());

    named_values_[&node] = alloca;
  } else {
    auto *global_var = new llvm::GlobalVariable(
        *module_, convertType(node.getType()), false,
        llvm::GlobalValue::InternalLinkage,
        llvm::Constant::getNullValue(convertType(node.getType())),
        node.getName());
    globals_[&node] = global_var;
  }
}

auto CodeGen::visitParmVarDecl(const ParmVarDecl &) -> void { /* nullop */ }
auto CodeGen::visitCompoundStmt(const CompoundStmt &node) -> void {
  for (const auto *declStmt : node.getDecls()) {
    declStmt->accept(*this);
  }

  for (const auto *stmt : node.getStmts()) {
    stmt->accept(*this);
  }
}

auto CodeGen::visitIfStmt(const IfStmt &node) -> void {
  node.getCond()->accept(*this);
  auto *cond_value = last_value_;
  if (!cond_value) {
    return;
  }

  llvm::Value *cond_bool = convertToBoolean(cond_value);

  auto *then_bb =
      llvm::BasicBlock::Create(*context_, "if.then", current_function_);
  auto *else_bb =
      llvm::BasicBlock::Create(*context_, "if.else", current_function_);
  auto *merge_bb =
      llvm::BasicBlock::Create(*context_, "if.end", current_function_);

  builder_->CreateCondBr(cond_bool, then_bb, else_bb);

  builder_->SetInsertPoint(then_bb);
  node.getThen()->accept(*this);
  if (!builder_->GetInsertBlock()->getTerminator()) {
    builder_->CreateBr(merge_bb);
  }

  builder_->SetInsertPoint(else_bb);
  if (node.getElse()) {
    node.getElse()->accept(*this);
  }
  if (!builder_->GetInsertBlock()->getTerminator()) {
    builder_->CreateBr(merge_bb);
  }

  builder_->SetInsertPoint(merge_bb);
}

auto CodeGen::visitWhileStmt(const WhileStmt &node) -> void {
  auto *cond_bb =
      llvm::BasicBlock::Create(*context_, "while.cond", current_function_);
  auto *body_bb =
      llvm::BasicBlock::Create(*context_, "while.body", current_function_);
  auto *end_bb =
      llvm::BasicBlock::Create(*context_, "while.end", current_function_);

  builder_->CreateBr(cond_bb);

  builder_->SetInsertPoint(cond_bb);
  node.getCond()->accept(*this);
  auto *cond_value = last_value_;

  if (!cond_value) {
    return;
  }

  llvm::Value *cond_bool = convertToBoolean(cond_value);

  builder_->CreateCondBr(cond_bool, body_bb, end_bb);

  builder_->SetInsertPoint(body_bb);
  node.getBody()->accept(*this);

  if (!builder_->GetInsertBlock()->getTerminator()) {
    builder_->CreateBr(cond_bb);
  }

  builder_->SetInsertPoint(end_bb);
}

auto CodeGen::visitReturnStmt(const ReturnStmt &node) -> void {
  if (node.getExpr()) {
    node.getExpr()->accept(*this);
    auto *ret_value = last_value_;

    if (ret_value) {
      builder_->CreateRet(ret_value);
    }
  } else {
    builder_->CreateRetVoid();
  }
}

auto CodeGen::visitDeclStmt(const DeclStmt &node) -> void {
  node.getDecl()->accept(*this);
}

auto CodeGen::visitCallExpr(const CallExpr &node) -> void {
  node.getCallee()->accept(*this);
  auto *callee_value = last_value_;

  if (!callee_value) {
    last_value_ = nullptr;
    return;
  }

  llvm::Function *func = nullptr;

  if (auto *func_ptr = llvm::dyn_cast<llvm::Function>(callee_value)) {
    func = func_ptr;
  } else {
    llvm::errs() << "error: indirect function calls not yet supported\n";
    last_value_ = nullptr;
    return;
  }

  std::vector<llvm::Value *> arg_values;
  arg_values.reserve(node.getArgs().size());

  for (const auto *arg : node.getArgs()) {
    arg->accept(*this);
    if (last_value_) {
      arg_values.push_back(last_value_);
    } else {
      last_value_ = nullptr;
      return;
    }
  }

  if (arg_values.size() != func->arg_size()) {
    llvm::errs() << "Error: Argument count mismatch in call to "
                 << func->getName() << "\n";
    last_value_ = nullptr;
    return;
  }

  last_value_ = builder_->CreateCall(func, arg_values, "calltmp");
}

auto CodeGen::visitParenExpr(const ParenExpr &node) -> void {
  node.getInner()->accept(*this);
}

auto CodeGen::visitDeclRefExpr(const DeclRefExpr &node) -> void {
  if (node.getRefType() == DeclRefExpr::FUNCTION) {
    auto *func = module_->getFunction(node.getName());
    if (!func) {
      llvm::errs() << "Error: Unknown function: " << node.getName() << "\n";
      last_value_ = nullptr;
      return;
    }
    last_value_ = func;
  } else {
    const auto *decl_ = node.getReference().value_or(nullptr);
    if (!decl_) {
      llvm::errs() << "Error: Unresolved reference: " << node.getName() << "\n";
      last_value_ = nullptr;
      return;
    }

    const auto *decl = llvm::dyn_cast<Decl>(decl_);
    auto local_it = named_values_.find(decl);
    if (local_it != named_values_.end()) {
      auto *var = local_it->second;

      if (node.getType()->getKind() == Type::TK_ARRAY) {
        last_value_ = var;
      } else if (llvm::isa<llvm::AllocaInst>(var)) {
        last_value_ = builder_->CreateLoad(convertType(node.getType()), var,
                                           node.getName());
      } else {
        last_value_ = var;
      }
      return;
    }

    auto global_it = globals_.find(decl);
    if (global_it != globals_.end()) {
      auto *var = global_it->second;

      if (node.getType()->getKind() == Type::TK_ARRAY) {
        last_value_ = var; // Arrays stay as pointers
      } else {
        last_value_ = builder_->CreateLoad(convertType(node.getType()), var,
                                           node.getName());
      }
      return;
    }

    llvm::errs() << "Error: Variable not in scope: " << node.getName() << "\n";
    last_value_ = nullptr;
  }
}

auto CodeGen::visitArraySubscriptExpr(const ArraySubscriptExpr &node) -> void {
  node.getArray()->accept(*this);
  auto *array_ptr = last_value_;

  if (!array_ptr) {
    last_value_ = nullptr;
    return;
  }

  node.getIndex()->accept(*this);
  auto *index_value = last_value_;

  if (!index_value) {
    last_value_ = nullptr;
    return;
  }

  llvm::Value *ptr = nullptr;
  llvm::Value *idx = nullptr;
  llvm::Type *element_type = convertType(node.getType());

  if (array_ptr->getType()->isPointerTy()) {
    ptr = array_ptr;
    idx = index_value;
  } else {
    ptr = index_value;
    idx = array_ptr;
  }

  last_value_ = builder_->CreateInBoundsGEP(element_type, ptr, idx, "arrayidx");
}

auto CodeGen::visitImplicitCastExpr(const ImplicitCastExpr &node) -> void {
  node.getExpr()->accept(*this);
  auto *value = last_value_;

  if (!value) {
    last_value_ = nullptr;
    return;
  }

  switch (node.getCastType()) {
  case CastType::LValueToRValue:
    if (value->getType()->isPointerTy()) {
      last_value_ = builder_->CreateLoad(convertType(node.getType()), value,
                                         "lvalueload");
    } else {
      last_value_ = value;
    }
    break;

  case CastType::ArrayToPointerDecay:
    if (value->getType()->isPointerTy()) {
      std::vector<llvm::Value *> indices = {
          llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 0),
          llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 0)};
      last_value_ = builder_->CreateInBoundsGEP(
          convertType(node.getExpr()->getType()), value, indices, "arraydecay");
    } else {
      last_value_ = value;
    }
    break;

  case CastType::FunctionToPointerDecay:
    last_value_ = value;
    break;

  case CastType::IntegralToFloat:
    last_value_ = builder_->CreateSIToFP(
        value, llvm::Type::getFloatTy(*context_), "itof");
    break;

  case CastType::BooleanToIntegral:
    last_value_ =
        builder_->CreateZExt(value, llvm::Type::getInt32Ty(*context_), "btoi");
    break;

  case CastType::BooleanToFloat:
    auto *as_int =
        builder_->CreateZExt(value, llvm::Type::getInt32Ty(*context_), "btoi");
    last_value_ = builder_->CreateSIToFP(
        as_int, llvm::Type::getFloatTy(*context_), "itof");
    break;
  }
}

auto CodeGen::visitBinaryOperator(const BinaryOperator &node) -> void {
  auto op = node.getOp();

  if (op == BinaryOperator::OP_ASSIGN) {
    last_value_ = handleAssignment(node.getLHS(), node.getRHS());
    return;
  }

  if (op == BinaryOperator::OP_AND || op == BinaryOperator::OP_OR) {
    last_value_ = createLogicalOp(op, node.getLHS(), node.getRHS());
    return;
  }

  node.getLHS()->accept(*this);
  auto *lhs = last_value_;

  node.getRHS()->accept(*this);
  auto *rhs = last_value_;

  if (!lhs || !rhs) {
    last_value_ = nullptr;
    return;
  }

  switch (op) {
  case BinaryOperator::OP_ADD:
  case BinaryOperator::OP_SUB:
  case BinaryOperator::OP_MUL:
  case BinaryOperator::OP_DIV:
  case BinaryOperator::OP_MOD:
    last_value_ = createArithmeticOp(op, lhs, rhs);
    break;

  case BinaryOperator::OP_EQ:
  case BinaryOperator::OP_NEQ:
  case BinaryOperator::OP_LT:
  case BinaryOperator::OP_LEQ:
  case BinaryOperator::OP_GT:
  case BinaryOperator::OP_GEQ:
    last_value_ = createComparisonOp(op, lhs, rhs);
    break;

  default:
    llvm::errs() << "error: unknown binary operator\n";
    last_value_ = nullptr;
    break;
  }
}

auto CodeGen::visitUnaryOperator(const UnaryOperator &node) -> void {
  node.getExpr()->accept(*this);

  auto *operand = last_value_;
  if (!operand) {
    last_value_ = nullptr;
    return;
  }

  switch (node.getOp()) {
  case UnaryOperator::OP_NEG: {
    if (operand->getType()->isFloatingPointTy()) {
      last_value_ = builder_->CreateFNeg(operand, "negtmp");
    } else if (operand->getType()->isIntegerTy(32)) {
      auto *zero = llvm::ConstantInt::get(operand->getType(), 0);
      last_value_ = builder_->CreateSub(zero, operand, "negtmp");
    } else {
      fmt::println("visited UnaryOperator OP_NEG with wrong type at {}",
                   node.getLocation().to_string());
    }
    break;
  }

  case UnaryOperator::OP_NOT: {
    if (operand->getType()->isIntegerTy(1)) {
      last_value_ = builder_->CreateNot(operand, "nottmp");
    } else {
      llvm::errs() << fmt::format(
          "visited UnaryOperator OP_NOT with wrong type at {} having type {}\n",
          node.getLocation().to_string(), node.getType()->to_string());
    }
    break;
  }

  default:
    last_value_ = nullptr;
    break;
  }
}

auto CodeGen::visitIntegerLiteral(const IntegerLiteral &node) -> void {
  last_value_ =
      llvm::ConstantInt::get(*context_, llvm::APInt(32, node.getLit(), true));
}
auto CodeGen::visitFloatLiteral(const FloatLiteral &node) -> void {
  last_value_ =
      llvm::ConstantFP::get(llvm::Type::getFloatTy(*context_), node.getLit());
}
auto CodeGen::visitBoolLiteral(const BoolLiteral &node) -> void {
  last_value_ = llvm::ConstantInt::get(convertType(node.getType()),
                                       node.getLit() ? 1 : 0, true);
}

auto CodeGen::convertType(const Type *type) -> llvm::Type * {
  assert(type);

  switch (type->getKind()) {
  case Type::TK_VOID: {
    return llvm::Type::getVoidTy(*context_);
  }
  case Type::TK_BOOL: {
    return llvm::Type::getInt1Ty(*context_);
  }
  case Type::TK_INT: {
    return llvm::Type::getInt32Ty(*context_);
  }
  case Type::TK_FLOAT: {
    return llvm::Type::getFloatTy(*context_);
  }
  case Type::TK_PTR: {
    auto *ptrType = static_cast<const PointerType *>(type);
    auto llvmPtr = elementToLLVMPtr.find(ptrType->getElementType());
    if (llvmPtr == elementToLLVMPtr.end()) {
      auto [newPtr, _] = elementToLLVMPtr.emplace(
          ptrType->getElementType(), llvm::PointerType::get(*context_, 0));
      llvmPtr = newPtr;
    }

    return llvmPtr->second;
  }
  case Type::TK_ARRAY: {
    auto *arrayType = static_cast<const ArrayType *>(type);

    llvm::Type *elementLLVMType = convertType(arrayType->getElementType());
    llvm::Type *currentType = elementLLVMType;

    const auto &dims = arrayType->getDimensions();
    for (auto it = dims.rbegin(); it != dims.rend(); ++it) {
      const Expr *dimExpr = *it;
      if (auto *intLit = llvm::dyn_cast<IntegerLiteral>(dimExpr)) {
        uint64_t size = intLit->getLit();
        currentType = llvm::ArrayType::get(currentType, size);
      } else {
        llvm::errs() << "Error: Array dimensions must be constant integers\n";
        return llvm::Type::getVoidTy(*context_);
      }
    }

    return currentType;
  }
  case Type::TK_FUNCTION: {
    auto *func_type = static_cast<const FunctionType *>(type);

    llvm::Type *return_type = convertType(func_type->getReturnType());

    std::vector<llvm::Type *> param_types;
    for (auto *arg_type : func_type->getArgTypes()) {
      param_types.push_back(convertType(arg_type));
    }

    return llvm::FunctionType::get(return_type, param_types, false);
  }
  }
}
auto CodeGen::createArithmeticOp(BinaryOperator::Operator op, llvm::Value *lhs,
                                 llvm::Value *rhs) -> llvm::Value * {
  if (!lhs || !rhs) {
    return nullptr;
  }

  bool is_float = lhs->getType()->isFloatingPointTy();

  switch (op) {
  case BinaryOperator::OP_ADD:
    return is_float ? builder_->CreateFAdd(lhs, rhs, "addtmp")
                    : builder_->CreateAdd(lhs, rhs, "addtmp");

  case BinaryOperator::OP_SUB:
    return is_float ? builder_->CreateFSub(lhs, rhs, "subtmp")
                    : builder_->CreateSub(lhs, rhs, "subtmp");

  case BinaryOperator::OP_MUL:
    return is_float ? builder_->CreateFMul(lhs, rhs, "multmp")
                    : builder_->CreateMul(lhs, rhs, "multmp");

  case BinaryOperator::OP_DIV:
    return is_float ? builder_->CreateFDiv(lhs, rhs, "divtmp")
                    : builder_->CreateSDiv(lhs, rhs, "divtmp");

  case BinaryOperator::OP_MOD:
    if (is_float) {
      llvm::errs() << "Error: Modulo operator not supported for floats\n";
      return nullptr;
    }
    return builder_->CreateSRem(lhs, rhs, "modtmp");

  default:
    llvm::errs() << "Error: Not an arithmetic operator\n";
    return nullptr;
  }
}

auto CodeGen::createComparisonOp(BinaryOperator::Operator op, llvm::Value *lhs,
                                 llvm::Value *rhs) -> llvm::Value * {
  if (!lhs || !rhs) {
    return nullptr;
  }

  bool is_float = lhs->getType()->isFloatingPointTy();

  if (is_float) {
    switch (op) {
    case BinaryOperator::OP_EQ:
      return builder_->CreateFCmpOEQ(lhs, rhs, "cmptmp");
    case BinaryOperator::OP_NEQ:
      return builder_->CreateFCmpONE(lhs, rhs, "cmptmp");
    case BinaryOperator::OP_LT:
      return builder_->CreateFCmpOLT(lhs, rhs, "cmptmp");
    case BinaryOperator::OP_LEQ:
      return builder_->CreateFCmpOLE(lhs, rhs, "cmptmp");
    case BinaryOperator::OP_GT:
      return builder_->CreateFCmpOGT(lhs, rhs, "cmptmp");
    case BinaryOperator::OP_GEQ:
      return builder_->CreateFCmpOGE(lhs, rhs, "cmptmp");
    default:
      llvm::errs() << "error: not a comparison operator\n";
      return nullptr;
    }
  } else {
    switch (op) {
    case BinaryOperator::OP_EQ:
      return builder_->CreateICmpEQ(lhs, rhs, "cmptmp");
    case BinaryOperator::OP_NEQ:
      return builder_->CreateICmpNE(lhs, rhs, "cmptmp");
    case BinaryOperator::OP_LT:
      return builder_->CreateICmpSLT(lhs, rhs, "cmptmp");
    case BinaryOperator::OP_LEQ:
      return builder_->CreateICmpSLE(lhs, rhs, "cmptmp");
    case BinaryOperator::OP_GT:
      return builder_->CreateICmpSGT(lhs, rhs, "cmptmp");
    case BinaryOperator::OP_GEQ:
      return builder_->CreateICmpSGE(lhs, rhs, "cmptmp");
    default:
      llvm::errs() << "error: not a comparison operator\n";
      return nullptr;
    }
  }
}

auto CodeGen::createLogicalOp(BinaryOperator::Operator op, const Expr *lhs_expr,
                              const Expr *rhs_expr) -> llvm::Value * {

  if (op == BinaryOperator::OP_AND) {
    auto *start_bb = builder_->GetInsertBlock();
    auto *rhs_bb =
        llvm::BasicBlock::Create(*context_, "and.rhs", current_function_);
    auto *merge_bb =
        llvm::BasicBlock::Create(*context_, "and.end", current_function_);

    lhs_expr->accept(*this);
    auto *lhs_val = last_value_;
    if (!lhs_val)
      return nullptr;

    auto *lhs_bool = convertToBoolean(lhs_val);

    builder_->CreateCondBr(lhs_bool, rhs_bb, merge_bb);
    builder_->SetInsertPoint(rhs_bb);
    rhs_expr->accept(*this);
    auto *rhs_val = last_value_;
    if (!rhs_val)
      return nullptr;
    auto *rhs_bool = convertToBoolean(rhs_val);
    auto *rhs_end_bb = builder_->GetInsertBlock();
    builder_->CreateBr(merge_bb);

    builder_->SetInsertPoint(merge_bb);
    auto *phi =
        builder_->CreatePHI(llvm::Type::getInt1Ty(*context_), 2, "andtmp");
    phi->addIncoming(llvm::ConstantInt::getFalse(*context_), start_bb);
    phi->addIncoming(rhs_bool, rhs_end_bb);

    return phi;

  } else if (op == BinaryOperator::OP_OR) {
    auto *start_bb = builder_->GetInsertBlock();
    auto *rhs_bb =
        llvm::BasicBlock::Create(*context_, "or.rhs", current_function_);
    auto *merge_bb =
        llvm::BasicBlock::Create(*context_, "or.end", current_function_);

    lhs_expr->accept(*this);
    auto *lhs_val = last_value_;
    if (!lhs_val)
      return nullptr;

    auto *lhs_bool = convertToBoolean(lhs_val);

    builder_->CreateCondBr(lhs_bool, merge_bb, rhs_bb);

    builder_->SetInsertPoint(rhs_bb);
    rhs_expr->accept(*this);
    auto *rhs_val = last_value_;
    if (!rhs_val)
      return nullptr;
    auto *rhs_bool = convertToBoolean(rhs_val);
    auto *rhs_end_bb = builder_->GetInsertBlock();
    builder_->CreateBr(merge_bb);

    builder_->SetInsertPoint(merge_bb);
    auto *phi =
        builder_->CreatePHI(llvm::Type::getInt1Ty(*context_), 2, "ortmp");
    phi->addIncoming(llvm::ConstantInt::getTrue(*context_), start_bb);
    phi->addIncoming(rhs_bool, rhs_end_bb);

    return phi;

  } else {
    llvm::errs() << "not a logical operator\n";
    return nullptr;
  }
}

auto CodeGen::handleAssignment(const Expr *lhs_expr, const Expr *rhs_expr)
    -> llvm::Value * {
  rhs_expr->accept(*this);
  auto *rhs_val = last_value_;
  if (!rhs_val)
    return nullptr;

  if (auto *decl_ref = llvm::dyn_cast<DeclRefExpr>(lhs_expr)) {
    const ASTNode *decl = decl_ref->getReference().value_or(nullptr);
    if (!decl) {
      llvm::errs() << "error: unresolved reference in assignment\n";
      return nullptr;
    }

    const auto *decl_ptr = llvm::dyn_cast<Decl>(decl);

    auto local_it = named_values_.find(decl_ptr);
    if (local_it != named_values_.end()) {
      auto *var = local_it->second;
      builder_->CreateStore(rhs_val, var);
      return rhs_val;
    }

    auto global_it = globals_.find(decl_ptr);
    if (global_it != globals_.end()) {
      auto *var = global_it->second;
      builder_->CreateStore(rhs_val, var);
      return rhs_val;
    }

    llvm::errs() << "variable not found in assignment\n";
    return nullptr;
  } else if (auto *array_sub = llvm::dyn_cast<ArraySubscriptExpr>(lhs_expr)) {
    array_sub->accept(*this);
    auto *element_ptr = last_value_;

    if (!element_ptr) {
      llvm::errs() << "error: failed to evaluate array subscript\n";
      return nullptr;
    }

    builder_->CreateStore(rhs_val, element_ptr);

    return rhs_val;

  } else {
    llvm::errs() << "error: assignment to non-variable not yet supported\n";
    return nullptr;
  }
}

auto CodeGen::convertToBoolean(llvm::Value *value) -> llvm::Value * {
  auto *value_type = value->getType();

  if (value_type->isIntegerTy(1)) {
    return value;
  }

  if (value_type->isIntegerTy()) {
    auto *zero = llvm::ConstantInt::get(value_type, 0);
    return builder_->CreateICmpNE(value, zero, "tobool");
  }

  if (value_type->isFloatingPointTy()) {
    auto *zero = llvm::ConstantFP::get(value_type, 0.0);
    return builder_->CreateFCmpONE(value, zero, "tobool");
  }

  llvm::errs() << "error: cannot convert value to boolean\n";
  return llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_), 0);
}

} // namespace mccomp
