#include "ast/_internal/type.h"
#include "error/error.h"
#include <semantic/type_checker.h>

namespace mccomp {

auto TypeChecker::convertNodeToNumeral(Expr *expr)
    -> tl::expected<Expr *, ClangError> {
  ImplicitCastExpr *cast;

  assert(expr->getType());
  switch (expr->getType()->getKind()) {
  case Type::TK_INT:
  case Type::TK_BOOL:
  case Type::TK_FLOAT:
  case Type::TK_PTR:
    return expr;
  case Type::TK_FUNCTION: {
    cast = ctx.create<ImplicitCastExpr>(expr, expr->getLocation());
    if (!cast)
      return cast;
    cast->setCastType(CastType::FunctionToPointerDecay);
    auto functionType = llvm::dyn_cast<FunctionType>(expr->getType());
    cast->setType(ctx.getPtrType(functionType));
    return cast;
  }
  case Type::TK_ARRAY: {
    cast = ctx.create<ImplicitCastExpr>(expr, expr->getLocation());
    if (!cast)
      return cast;
    cast->setCastType(CastType::ArrayToPointerDecay);
    auto arrType = llvm::dyn_cast<ArrayType>(expr->getType());
    cast->setType(ctx.getPtrTypeFromArrayType(arrType));

    return cast;
  }
  case Type::TK_VOID: {
    return tl::unexpected(ClangError(ClangErrorSeverity::ERROR,
                                     "unknown cast from void to numeral"));
  }
  }
}

auto TypeChecker::visitTranslationUnit(TranslationUnit &node) -> void {
  auto externs = node.getExterns();
  auto decls = node.getDecls();

  for (auto externNode : externs) {
    externNode->accept(*this);
  }

  for (auto declNode : decls) {
    declNode->accept(*this);
  }
}

auto TypeChecker::visitFunctionDecl(FunctionDecl &node) -> void {
  auto symbol = Symbol(Symbol::SymbolType::Func, node.getName(), &node,
                       node.getFunctionType());
  auto tryBuild = this->symbolTable.addSymbol(symbol);
  if (!tryBuild) {
    auto isRedefinition = tryBuild.error().get().type == node.getFunctionType();
    node.invalidate();
    if (isRedefinition) {
      auto error = ClangError(
          ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
          node.getIdent().getLineNo(), node.getIdent().getColumnNo(),
          fmt::format("redefinition of \'{}\'", node.getName()));

      fmt::println("{}", error.to_string());
      success = false;
      node.invalidate();
      return;
    } else {
      auto error = ClangError(
          ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
          node.getIdent().getLineNo(), node.getIdent().getColumnNo(),
          fmt::format("conflicting types for \'{}\'", node.getName()));

      fmt::println("{}", error.to_string());
      success = false;
      node.invalidate();
      return;
    }
  }

  this->symbolTable.pushFrame();
  for (auto param : node.getParams()) {
    param->accept(*this);
  }
  this->symbolTable.bufferFrame(
      std::move(this->symbolTable.popFrame().value()));

  auto oldReturnType = this->scopeReturnType;
  this->scopeReturnType = {node.getFunctionType()->getReturnType(),
                           node.getName()};

  if (node.getBody()) {
    node.getBody()->accept(*this);
  }

  this->scopeReturnType = oldReturnType;
  node.setType(node.getFunctionType()->getReturnType());
}

auto TypeChecker::visitVarDecl(VarDecl &node) -> void {
  auto symbol =
      Symbol(Symbol::SymbolType::Func, node.getName(), &node, node.getType());

  auto tryBuild = this->symbolTable.addSymbol(symbol);
  if (!tryBuild) {
    node.invalidate();
    auto fmtStr = fmt::format("redefinition of \'{}\'", node.getName());
    auto sameType = tryBuild.error().get().type == node.getType();
    if (sameType) {
      fmtStr += fmt::format(" with a different type: \'{}\' vs \'{}\'",
                            node.getType()->to_string(),
                            tryBuild.error().get().type->to_string());
    }

    auto error = ClangError(ClangErrorSeverity::ERROR,
                            node.getLocation().fileName.value(),
                            node.getIdent().getLineNo(),
                            node.getIdent().getColumnNo(), std::move(fmtStr));
    fmt::println("{}", error.to_string());
    success = false;
  }
}

auto TypeChecker::visitParmVarDecl(ParmVarDecl &node) -> void {
  auto symbol =
      Symbol(Symbol::SymbolType::Func, node.getName(), &node, node.getType());

  auto tryBuild = this->symbolTable.addSymbol(symbol);
  if (!tryBuild) {
    node.invalidate();
    auto fmtStr = fmt::format("redefinition of \'{}\'", node.getName());
    auto sameType = tryBuild.error().get().type == node.getType();
    if (!sameType) {
      fmtStr += fmt::format(" with a different type: \'{}\' vs \'{}\'",
                            node.getType()->to_string(),
                            tryBuild.error().get().type->to_string());
    }

    auto error = ClangError(ClangErrorSeverity::ERROR,
                            node.getLocation().fileName.value(),
                            node.getIdent().getLineNo(),
                            node.getIdent().getColumnNo(), std::move(fmtStr));
    fmt::println("{}", error.to_string());
    success = false;
  }
}

auto TypeChecker::visitCompoundStmt(CompoundStmt &node) -> void {
  this->symbolTable.pushFrame();

  for (auto decl : node.getDecls()) {
    decl->accept(*this);
  }

  for (auto stmt : node.getStmts()) {
    stmt->accept(*this);
  }

  this->symbolTable.popFrame();
}

auto TypeChecker::visitIfStmt(IfStmt &node) -> void {
  auto condition = node.getCond();
  condition->accept(*this);
  auto castToNumeral = convertNodeToNumeral(condition);
  if (!castToNumeral) {
    fmt::println("{}", castToNumeral.error().to_string());
    success = false;
  } else {
    node.setCond(castToNumeral.value());
  }

  node.getThen()->accept(*this);
  if (node.getElse()) {
    node.getElse()->accept(*this);
  }
}

auto TypeChecker::visitWhileStmt(WhileStmt &node) -> void {
  auto condition = node.getCond();
  condition->accept(*this);
  auto castToNumeral = convertNodeToNumeral(condition);
  if (!castToNumeral) {
    fmt::println("{}", castToNumeral.error().to_string());
    success = false;
  } else {
    node.setCond(castToNumeral.value());
  }

  node.getBody()->accept(*this);
}

auto TypeChecker::visitReturnStmt(ReturnStmt &node) -> void {
  assert(this->scopeReturnType.has_value());
  auto expectVoid =
      this->scopeReturnType.value().first->getKind() == Type::TK_VOID;
  if (!expectVoid && !node.getExpr()) {
    auto error = ClangError(
        ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
        node.getLocation().startLineNo, node.getLocation().endLineNo,
        fmt::format("non-void function '{}' should return a value",
                    scopeReturnType.value().second));
    fmt::println("{}", error);
    this->success = false;
    return;
  }

  node.getExpr()->accept(*this);
  if (!node.getExpr()->isValid())
    return;

  // cheating but lets say only allow int/float/bool return as per grammar
  auto canCast =
      node.getExpr()->getType()->convertsTo(scopeReturnType->first, true);
  if (!canCast) {
    auto error = ClangError(
        ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
        node.getExpr()->getLocation().startLineNo,
        node.getExpr()->getLocation().startColumnNo, canCast.error().c_str());
    fmt::println("{}", error);
    this->success = false;
    return;
  } else if (canCast && canCast->has_value()) {
    auto castType = canCast->value();
    auto newNode =
        ctx.create<ImplicitCastExpr>(node.getExpr(), node.getLocation());
    newNode->setCastType(castType);

    // cheese TODO: this is pretty unsafe
    newNode->setType((castType == CastType::BooleanToIntegral)
                         ? ctx.getIntType()
                         : ctx.getFloatType());
    node.setExpr(newNode);
  }
}

auto TypeChecker::visitDeclStmt(DeclStmt &node) -> void {
  node.getDecl()->accept(*this);
}

auto TypeChecker::visitCallExpr(CallExpr &node) -> void {
  node.getCallee()->accept(*this);
  for (auto arg : node.getArgs()) {
    arg->accept(*this);
  }

  for (const auto &arg : node.getArgs()) {
    if (!arg->isValid()) {
      node.invalidate();
      return;
    }
  }

  if (!node.getCallee()->isValid()) {
    node.invalidate();
    return;
  }

  if (node.getCallee()->getKind() != ASTNode::NK_DeclRefExpr) {
    auto error = ClangError(
        ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
        node.getLocation().startLineNo, node.getLocation().endLineNo,
        fmt::format(
            "called object type '{}' is not a function or a function pointer",
            node.getCallee()->getType()->to_string()));
    node.invalidate();
    this->success = false;
  }

  auto callee = llvm::dyn_cast<DeclRefExpr>(node.getCallee());
  assert(callee->getReference().has_value());
  assert(llvm::isa<FunctionDecl>(callee->getReference().value()));
  auto fnDecl = llvm::dyn_cast<FunctionDecl>(callee->getReference().value());
  auto fnArgs = fnDecl->getFunctionType()->getArgTypes();
  auto &calledArgs = node.getArgs();

  if (fnArgs.size() != calledArgs.size()) {
    auto error = ClangError(
        ClangErrorSeverity::ERROR, callee->getLocation().fileName.value(),
        callee->getLocation().startLineNo, callee->getLocation().endLineNo,
        fmt::format("too {} arguments to function call, expected {}, have {}",
                    (fnArgs.size() > calledArgs.size() ? "few" : "many"),
                    fnArgs.size(), calledArgs.size()));
    fmt::println("{}", error);
    this->success = false;
    node.setType(fnDecl->getType());
    return;
  }

  for (auto i = std::size_t{}; i < fnArgs.size(); ++i) {
    auto canCast = calledArgs[i]->getType()->convertsTo(fnArgs[i], false);
    if (!canCast) {
      auto error = ClangError(ClangErrorSeverity::ERROR,
                              calledArgs[i]->getLocation().fileName.value(),
                              calledArgs[i]->getLocation().startLineNo,
                              calledArgs[i]->getLocation().startColumnNo,
                              canCast.error().c_str());
      fmt::println("{}", error);
      this->success = false;
      node.setType(fnDecl->getType());
      return;
    }

    if (canCast->has_value()) {
      auto newNode = ctx.create<ImplicitCastExpr>(calledArgs[i],
                                                  calledArgs[i]->getLocation());
      newNode->setCastType(canCast->value());
      newNode->setType(fnArgs[i]);
      newNode->setValueCategory(Expr::RValue);

      calledArgs[i] = newNode;
    }
  }

  // then decay function type
  auto newType = ctx.getPtrType(fnDecl->getFunctionType());
  auto newNode = decayFunction(callee);

  node.setCallee(newNode);
  node.setType(fnDecl->getType());
}

auto TypeChecker::visitParenExpr(ParenExpr &node) -> void {
  node.getInner()->accept(*this);
  if (!node.getInner()->isValid()) {
    node.invalidate();
    return;
  } else {
    node.setType(node.getInner()->getType());
  }
}

auto TypeChecker::visitDeclRefExpr(DeclRefExpr &node) -> void {
  auto symbol = this->symbolTable.find(node.getName());
  const auto &ident = node.ident;
  if (!symbol.has_value()) {
    if (node.getRefType() == DeclRefExpr::FUNCTION) {
      auto error = ClangError(
          ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
          ident.getLineNo(), ident.getColumnNo(),
          std::make_tuple(ident.getLineNo(), ident.getColumnNo(),
                          ident.getColumnNo() + ident.getLexeme().size()),
          fmt::format("call to undeclared function '{}'; ISO C99 and later do "
                      "not support implicit function declarations",
                      ident.getLexeme()));
      fmt::println("{}", error);
    } else {
      auto error = ClangError(
          ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
          ident.getLineNo(), ident.getColumnNo(),
          std::make_tuple(ident.getLineNo(), ident.getColumnNo(),
                          ident.getColumnNo() + ident.getLexeme().size()),
          fmt::format("use of undeclared identifier '{}'", ident.getLexeme()));
      fmt::println("{}", error);
    }

    this->success = false;
    node.invalidate();
    return;
  }

  if (node.getRefType() == DeclRefExpr::FUNCTION &&
      symbol->get().symbolType == Symbol::Var) {
    auto error = ClangError(
        ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
        ident.getLineNo(), ident.getColumnNo(),
        std::make_tuple(ident.getLineNo(), ident.getColumnNo(),
                        ident.getColumnNo() + ident.getLexeme().size()),
        fmt::format(
            "called object type '{}' is not a function or function pointer",
            symbol->get().type->to_string()));
    fmt::println("{}", error);
    this->success = false;
    node.invalidate();
    return;
  }

  node.setReference(symbol->get().decl);
  node.setType(symbol->get().type);
  node.setValueCategory(Expr::LValue);
}

// TODO: implement
auto TypeChecker::visitArraySubscriptExpr(ArraySubscriptExpr &node) -> void {
  node.getArray()->accept(*this);
  node.getIndex()->accept(*this);
  if (!node.getArray()->isValid() || !node.getIndex()->isValid()) {
    node.invalidate();
    success = false;
    return;
  }

  if (llvm::isa<FunctionType>(node.getArray()->getType())) {
    auto error = ClangError(
        ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
        node.getArray()->getLocation().startLineNo,
        node.getArray()->getLocation().startColumnNo,
        fmt::format("subscript of pointer to function type '{}'",
                    llvm::dyn_cast<FunctionType>(node.getArray()->getType())
                        ->to_string()));
    fmt::println("{}", error);
    success = false;
    node.invalidate();
    return;
  } else if (llvm::isa<FunctionType>(node.getIndex()->getType())) {
    auto error = ClangError(
        ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
        node.getIndex()->getLocation().startLineNo,
        node.getIndex()->getLocation().startColumnNo,
        fmt::format("subscript of pointer to function type '{}'",
                    llvm::dyn_cast<FunctionType>(node.getArray()->getType())
                        ->to_string()));
    fmt::println("{}", error);
    success = false;
    node.invalidate();
    return;
  }

  auto isIndexable = [](const Type *t) -> bool {
    return t->getKind() == Type::TK_ARRAY || t->getKind() == Type::TK_PTR;
  };

  auto isIndex = [](const Type *t) -> bool {
    return t->getKind() == Type::TK_INT || t->getKind() == Type::TK_BOOL;
  };

  auto bothIndexable = isIndexable(node.getArray()->getType()) &&
                       isIndexable(node.getIndex()->getType());
  auto neitherIndexable = !isIndexable(node.getArray()->getType()) &&
                          !isIndexable(node.getIndex()->getType());
  auto hasIndex = isIndex(node.getArray()->getType()) ||
                  isIndex(node.getIndex()->getType());
  if (bothIndexable) {
    auto error = ClangError(ClangErrorSeverity::ERROR,
                            node.getLocation().fileName.value(),
                            node.getIndex()->getLocation().startLineNo,
                            node.getIndex()->getLocation().startColumnNo,
                            "array subscript is not an integer");
    fmt::println("{}", error);
    node.invalidate();
    success = false;
    return;
  } else if (neitherIndexable) {
    auto error = ClangError(ClangErrorSeverity::ERROR,
                            node.getLocation().fileName.value(),
                            node.getIndex()->getLocation().startLineNo,
                            node.getIndex()->getLocation().startColumnNo,
                            "subscripted value is not an array or pointer");
    fmt::println("{}", error);
    node.invalidate();
    success = false;
    return;
  } else if (!hasIndex) {
    auto error = ClangError(
        ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
        node.getIndex()->getLocation().startLineNo,
        node.getIndex()->getLocation().startColumnNo,
        fmt::format("invalid subscript of type '{}'",
                    node.getIndex()->getType()->to_string()));
    fmt::println("{}", error);
    node.invalidate();
    success = false;
    return;
  }

  if (node.getArray()->getType()->getKind() == Type::TK_BOOL) {
    auto promote = promoteBoolToInt(node.getArray());
    node.setArray(promote);
  }

  if (node.getIndex()->getType()->getKind() == Type::TK_BOOL) {
    auto promote = promoteBoolToInt(node.getIndex());
    node.setIndex(promote);
  }

  if (llvm::isa<ArrayType>(node.getArray()->getType())) {
    auto newNode = decayArray(node.getArray());
    node.setArray(newNode);
    assert(llvm::isa<PointerType>(newNode->getType()));
    node.setType(
        llvm::dyn_cast<PointerType>(newNode->getType())->getElementType());
    node.setValueCategory(node.getArray()->getValueCategory().value());
  } else if (llvm::isa<ArrayType>(node.getIndex()->getType())) {
    auto newNode = decayArray(node.getIndex());
    node.setIndex(newNode);
    assert(llvm::isa<PointerType>(newNode->getType()));
    node.setType(
        llvm::dyn_cast<PointerType>(newNode->getType())->getElementType());
    node.setValueCategory(node.getIndex()->getValueCategory().value());
  } else if (llvm::isa<PointerType>(node.getArray()->getType())) {
    node.setType(llvm::dyn_cast<PointerType>(node.getArray()->getType())
                     ->getElementType());
    node.setValueCategory(node.getArray()->getValueCategory().value());
  } else if (llvm::isa<PointerType>(node.getIndex()->getType())) {
    node.setType(llvm::dyn_cast<PointerType>(node.getIndex()->getType())
                     ->getElementType());
    node.setValueCategory(node.getIndex()->getValueCategory().value());
  }
}

auto TypeChecker::visitImplicitCastExpr(ImplicitCastExpr &node) -> void {
  // we need to set this on construction
  //
  // dont traverse because to construct it we traversed children already....
  //
  // we can just assert postconditions of a traversal of this node
  assert(node.getType());
  assert(node.getValueCategory());
  assert(static_cast<int>(node.getCastType()) >= 0);
}

auto TypeChecker::visitBinaryOperator(BinaryOperator &node) -> void {
  node.getLHS()->accept(*this);
  node.getRHS()->accept(*this);

  if (!node.getLHS()->isValid() || !node.getRHS()->isValid()) {
    node.invalidate();
    return;
  }

  auto &opTok = node.getOpToken();
  auto lhsLoc = node.getLHS()->getLocation();
  auto rhsLoc = node.getRHS()->getLocation();

  if (node.getLHS()->getType()->getKind() == Type::TK_VOID ||
      node.getRHS()->getType()->getKind() == Type::TK_VOID) {
    auto error = ClangError(
        ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
        node.getOpToken().getLineNo(), node.getOpToken().getColumnNo(),
        fmt::format("invalid operands to binary expression ('{}' and '{}')",
                    node.getLHS()->getType()->to_string(),
                    node.getRHS()->getType()->to_string()));
    fmt::println("{}", error);
    node.invalidate();
    success = false;
    return;
  }

  switch (node.getOp()) {
  case BinaryOperator::OP_ASSIGN: {
    if (node.getLHS()->getValueCategory() != Expr::LValue) {
      auto error = ClangError(
          ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
          opTok.getLineNo(), opTok.getColumnNo(),
          std::make_tuple(lhsLoc.startLineNo, lhsLoc.startColumnNo,
                          lhsLoc.endColumnNo),
          "expression is not assignable");
      fmt::println("{}", error);
      node.invalidate();
      success = false;
      return;
    } else {
      auto canCast =
          node.getRHS()->getType()->convertsTo(node.getLHS()->getType(), false);
      if (!canCast) {
        auto error =
            ClangError(ClangErrorSeverity::ERROR, rhsLoc.fileName.value(),
                       opTok.getLineNo(), opTok.getColumnNo(),
                       std::make_tuple(rhsLoc.endLineNo, rhsLoc.startColumnNo,
                                       rhsLoc.endColumnNo),
                       canCast.error().c_str());
        fmt::println("{}", error);
        this->success = false;
        node.invalidate();
        return;
      }

      if (canCast->has_value()) {
        auto newNode = ctx.create<ImplicitCastExpr>(
            node.getRHS(), node.getRHS()->getLocation());
        newNode->setCastType(canCast->value());
        newNode->setType(node.getLHS()->getType());
        node.setRHS(newNode);
      }

      node.setType(node.getLHS()->getType());
      node.setValueCategory(node.getRHS()->getValueCategory().value());
    }
    return;
  }
  // basic arithmetic
  case BinaryOperator::OP_MUL:
  case BinaryOperator::OP_DIV:
  case BinaryOperator::OP_ADD:
  case BinaryOperator::OP_SUB: {
    auto lhsType = node.getLHS()->getType(), rhsType = node.getRHS()->getType();
    if (llvm::isa<FunctionType>(lhsType)) {
      auto decayed = decayFunction(node.getLHS());
      node.setLHS(decayed);
    } else if (llvm::isa<ArrayType>(lhsType)) {
      auto decayed = decayArray(node.getLHS());
      node.setLHS(decayed);
    }

    if (llvm::isa<FunctionType>(rhsType)) {
      auto decayed = decayFunction(node.getRHS());
      node.setRHS(decayed);
    } else if (llvm::isa<ArrayType>(rhsType)) {
      auto decayed = decayArray(node.getRHS());
      node.setRHS(decayed);
    }

    auto hasPointer = llvm::isa<PointerType>(node.getLHS()->getType()) ||
                      llvm::isa<PointerType>(node.getRHS()->getType());
    auto bothPointer = llvm::isa<PointerType>(node.getLHS()->getType()) &&
                       llvm::isa<PointerType>(node.getRHS()->getType());
    auto hasFloat = (node.getLHS()->getType()->getKind() == Type::TK_FLOAT) ||
                    (node.getRHS()->getType()->getKind() == Type::TK_FLOAT);
    auto hasBool = (node.getLHS()->getType()->getKind() == Type::TK_BOOL) ||
                   (node.getRHS()->getType()->getKind() == Type::TK_BOOL);
    auto isMulDiv = node.getOp() == BinaryOperator::OP_MUL ||
                    node.getOp() == BinaryOperator::OP_DIV;

    auto invalid =
        (hasPointer && isMulDiv) ||
        (node.getOp() == BinaryOperator::OP_ADD && bothPointer) ||
        (node.getOp() == BinaryOperator::OP_SUB &&
         (hasPointer && llvm::isa<PointerType>(node.getRHS()->getType()))) ||
        (hasPointer && hasFloat);

    if (invalid) {
      auto error = ClangError(
          ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
          node.getOpToken().getLineNo(), node.getOpToken().getColumnNo(),
          fmt::format("invalid operands to binary expression ('{}' and '{}')",
                      node.getLHS()->getType()->to_string(),
                      node.getRHS()->getType()->to_string()));
      fmt::println("{}", error);
      node.invalidate();
      success = false;
      return;
    }

    if (hasBool) {
      if (node.getLHS()->getType()->getKind() == Type::TK_BOOL) {
        auto promote = promoteBoolToInt(node.getLHS());
        node.setLHS(promote);
      }

      if (node.getRHS()->getType()->getKind() == Type::TK_BOOL) {
        auto promote = promoteBoolToInt(node.getRHS());
        node.setRHS(promote);
      }
    }

    auto hasInt = (node.getLHS()->getType()->getKind() == Type::TK_INT) ||
                  (node.getRHS()->getType()->getKind() == Type::TK_INT);
    if (hasFloat && hasInt) {
      if (node.getLHS()->getType()->getKind() == Type::TK_INT) {
        auto promote = promoteInt(node.getLHS());
        node.setLHS(promote);
      } else {
        auto promote = promoteInt(node.getRHS());
        node.setRHS(promote);
      }
    }

    if (hasFloat) {
      node.setType(ctx.getFloatType());
    } else {
      node.setType(ctx.getIntType());
    }
    node.setValueCategory(Expr::RValue);
  }
    // mod operators
  case BinaryOperator::OP_MOD: {
    auto isIntegral = [](const Type *t) -> bool {
      return (t->getKind() == Type::TK_INT) || (t->getKind() == Type::TK_BOOL);
    };
    auto valid = isIntegral(node.getLHS()->getType()) &&
                 isIntegral(node.getRHS()->getType());

    if (!valid) {
      auto error = ClangError(
          ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
          node.getOpToken().getLineNo(), node.getOpToken().getColumnNo(),
          fmt::format("invalid operands to binary expression ('{}' and '{}')",
                      node.getLHS()->getType()->to_string(),
                      node.getRHS()->getType()->to_string()));
      fmt::println("{}", error);
      node.invalidate();
      success = false;
      return;
    }

    if (node.getLHS()->getType()->getKind() == Type::TK_BOOL) {
      auto promote = promoteBoolToInt(node.getLHS());
      node.setLHS(promote);
    }

    if (node.getRHS()->getType()->getKind() == Type::TK_BOOL) {
      auto promote = promoteBoolToInt(node.getRHS());
      node.setRHS(promote);
    }

    node.setType(ctx.getIntType());
    node.setValueCategory(Expr::RValue);
    return;
  }
    // boolean only operators
  case BinaryOperator::OP_AND:
  case BinaryOperator::OP_OR: {
    auto valid = node.getLHS()->getType()->getKind() == Type::TK_BOOL &&
                 node.getRHS()->getType()->getKind() == Type::TK_BOOL;
    auto hasFloat = (node.getLHS()->getType()->getKind() == Type::TK_FLOAT) ||
                    (node.getRHS()->getType()->getKind() == Type::TK_FLOAT);
    auto hasInt = (node.getLHS()->getType()->getKind() == Type::TK_INT) ||
                  (node.getRHS()->getType()->getKind() == Type::TK_INT);
    if (!valid && !(hasFloat || hasInt)) {
      auto error = ClangError(
          ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
          node.getOpToken().getLineNo(), node.getOpToken().getColumnNo(),
          fmt::format("invalid operands to binary expression ('{}' and '{}')",
                      node.getLHS()->getType()->to_string(),
                      node.getRHS()->getType()->to_string()));
      fmt::println("{}", error);
      success = false;
      node.invalidate();
      return;
    } else if (!valid && (hasFloat || hasInt)) {
      auto error = ClangError(
          ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
          node.getOpToken().getLineNo(), node.getOpToken().getColumnNo(),
          fmt::format("implicit (narrowing) conversion to '{}' from '{}')",
                      ctx.getBoolType()->to_string(),
                      (hasInt ? "int" : "float")));
      fmt::println("{}", error);
      node.invalidate();
      success = false;
      return;
    }

    node.setType(ctx.getBoolType());
    node.setValueCategory(Expr::RValue);
    return;
  }
    // comparators
  case BinaryOperator::OP_LEQ:
  case BinaryOperator::OP_LT:
  case BinaryOperator::OP_GEQ:
  case BinaryOperator::OP_GT:
  case BinaryOperator::OP_EQ:
  case BinaryOperator::OP_NEQ: {
    auto isFuncOrArray = [](const Type *type) -> bool {
      return (type->getKind() == Type::TK_ARRAY) ||
             (type->getKind() == Type::TK_FUNCTION);
    };

    auto bothFuncOrArray = isFuncOrArray(node.getLHS()->getType()) &&
                           isFuncOrArray(node.getRHS()->getType());
    if (bothFuncOrArray) {
      auto error = ClangError(
          ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
          node.getOpToken().getLineNo(), node.getOpToken().getColumnNo(),
          fmt::format("invalid operands to binary expression ('{}' and '{}')",
                      node.getLHS()->getType()->to_string(),
                      node.getRHS()->getType()->to_string()));
      fmt::println("{}", error);
      node.invalidate();
      success = false;
      return;
    }

    if (llvm::isa<FunctionType>(node.getLHS()->getType())) {
      auto decayed = decayFunction(node.getLHS());
      node.setLHS(decayed);
    } else if (llvm::isa<ArrayType>(node.getLHS()->getType())) {
      auto decayed = decayArray(node.getLHS());
      node.setLHS(decayed);
    }

    if (llvm::isa<FunctionType>(node.getRHS()->getType())) {
      auto decayed = decayFunction(node.getRHS());
      node.setRHS(decayed);
    } else if (llvm::isa<ArrayType>(node.getRHS()->getType())) {
      auto decayed = decayArray(node.getRHS());
      node.setRHS(decayed);
    }

    auto hasFloat = (node.getLHS()->getType()->getKind() == Type::TK_FLOAT) ||
                    (node.getRHS()->getType()->getKind() == Type::TK_FLOAT);
    auto hasBool = (node.getLHS()->getType()->getKind() == Type::TK_BOOL) ||
                   (node.getRHS()->getType()->getKind() == Type::TK_BOOL);
    auto bothBool = (node.getLHS()->getType()->getKind() == Type::TK_BOOL) &&
                    (node.getRHS()->getType()->getKind() == Type::TK_BOOL);
    auto bothInt = (node.getLHS()->getType()->getKind() == Type::TK_INT) &&
                   (node.getRHS()->getType()->getKind() == Type::TK_INT);
    auto bothPtr = (node.getLHS()->getType()->getKind() == Type::TK_PTR) &&
                   (node.getRHS()->getType()->getKind() == Type::TK_PTR);

    if (bothPtr)
      return;
    else if (bothInt)
      return;
    else if (bothBool)
      return;

    if (hasBool) {
      if (node.getLHS()->getType()->getKind() == Type::TK_BOOL) {
        auto promote = promoteBoolToInt(node.getLHS());
        node.setLHS(promote);
      }

      if (node.getRHS()->getType()->getKind() == Type::TK_BOOL) {
        auto promote = promoteBoolToInt(node.getRHS());
        node.setRHS(promote);
      }
    }

    auto hasInt = (node.getLHS()->getType()->getKind() == Type::TK_INT) ||
                  (node.getRHS()->getType()->getKind() == Type::TK_INT);
    if (hasFloat && hasInt) {
      if (node.getLHS()->getType()->getKind() == Type::TK_INT) {
        auto promote = promoteInt(node.getLHS());
        node.setLHS(promote);
      } else {
        auto promote = promoteInt(node.getRHS());
        node.setRHS(promote);
      }
    }

    node.setType(ctx.getBoolType());
    node.setValueCategory(Expr::RValue);
    return;
  }
  }
}

auto TypeChecker::visitUnaryOperator(UnaryOperator &node) -> void {
  auto expr = node.getExpr();
  expr->accept(*this);

  if (expr->getValueCategory() == Expr::LValue) {
    auto setType = lValueToRValue(expr);
    node.setExpr(setType);
  }

  if (node.getOp() == UnaryOperator::OP_NOT) {
    auto canCast = expr->getType()->convertsTo(ctx.getBoolType(), false);
    if (!canCast) {
      auto error = ClangError(ClangErrorSeverity::ERROR,
                              node.getLocation().fileName.value(),
                              node.getExpr()->getLocation().startLineNo,
                              node.getExpr()->getLocation().startColumnNo,
                              std::string(canCast.error()));
      fmt::println("{}", error);
      success = false;
      node.setType(ctx.getBoolType());
      return;
    }

    node.setType(ctx.getBoolType());
    node.setValueCategory(Expr::RValue);
  } else {
    // OP_NEG
    if (expr->getType()->getKind() == Type::TK_BOOL) {
      auto canCast = node.getType()->convertsTo(ctx.getIntType(), false);
      assert(canCast);

      auto newNode = ctx.create<ImplicitCastExpr>(expr, expr->getLocation());
      newNode->setCastType(canCast->value());
      newNode->setType(ctx.getIntType());
      node.setExpr(newNode);
    } else if (expr->getType()->getKind() == Type::TK_PTR ||
               expr->getType()->getKind() == Type::TK_FUNCTION ||
               expr->getType()->getKind() == Type::TK_ARRAY) {
      auto error = ClangError(
          ClangErrorSeverity::ERROR, node.getLocation().fileName.value(),
          expr->getLocation().startLineNo, expr->getLocation().startColumnNo,
          fmt::format("invalid argument type \'{}\' to unary expression",
                      expr->getType()->to_string()));
      fmt::println("{}", error.to_string());
      success = false;
      node.invalidate();
      return;
    } else {
      assert(expr->getType()->getKind() != Type::TK_VOID);
    }
    node.setType(expr->getType());
    node.setValueCategory(Expr::RValue);
  }
}

auto TypeChecker::visitIntegerLiteral(IntegerLiteral &node) -> void {
  node.setType(ctx.getIntType());
  node.setValueCategory(Expr::RValue);
}

auto TypeChecker::visitFloatLiteral(FloatLiteral &node) -> void {
  node.setType(ctx.getFloatType());
  node.setValueCategory(Expr::RValue);
}

auto TypeChecker::visitBoolLiteral(BoolLiteral &node) -> void {
  node.setType(ctx.getBoolType());
  node.setValueCategory(Expr::RValue);
}

auto TypeChecker::decayFunction(Expr *expr) -> Expr * {
  assert(expr);
  assert(expr->getType());
  assert(llvm::isa<FunctionType>(expr->getType()));

  auto newType = ctx.getPtrType(expr->getType());
  auto newNode = ctx.create<ImplicitCastExpr>(expr, expr->getLocation());
  newNode->setCastType(CastType::FunctionToPointerDecay);
  newNode->setType(newType);
  newNode->setValueCategory(Expr::RValue);
  return newNode;
}

auto TypeChecker::decayArray(Expr *expr) -> Expr * {
  assert(expr);
  assert(expr->getType());
  assert(llvm::isa<ArrayType>(expr->getType()));

  auto oldCategory = expr->getValueCategory();
  auto newType =
      ctx.getPtrTypeFromArrayType(llvm::dyn_cast<ArrayType>(expr->getType()));
  auto newNode = ctx.create<ImplicitCastExpr>(expr, expr->getLocation());
  newNode->setCastType(CastType::FunctionToPointerDecay);
  newNode->setType(newType);
  newNode->setValueCategory(oldCategory.value());

  return newNode;
}

auto TypeChecker::lValueToRValue(Expr *expr) -> Expr * {
  assert(expr);
  assert(expr->getType());
  assert(expr->getValueCategory() == Expr::LValue);

  auto newNode = ctx.create<ImplicitCastExpr>(expr, expr->getLocation());
  assert(newNode);
  newNode->setCastType(CastType::LValueToRValue);
  newNode->setType(expr->getType());
  newNode->setValueCategory(Expr::RValue);

  return newNode;
}

auto TypeChecker::promoteInt(Expr *expr) -> Expr * {
  assert(expr);
  assert(expr->getType());
  assert(expr->getType()->getKind() == Type::TK_INT);

  auto newType = ctx.getFloatType();
  auto newNode = ctx.create<ImplicitCastExpr>(expr, expr->getLocation());
  newNode->setCastType(CastType::IntegralToFloat);
  newNode->setType(newType);
  newNode->setValueCategory(Expr::RValue);
  return newNode;
}

auto TypeChecker::promoteBoolToFloat(Expr *expr) -> Expr * {
  assert(expr);
  assert(expr->getType());
  assert(expr->getType()->getKind() == Type::TK_BOOL);

  auto newType = ctx.getFloatType();
  auto newNode = ctx.create<ImplicitCastExpr>(expr, expr->getLocation());
  newNode->setCastType(CastType::BooleanToFloat);
  newNode->setType(newType);
  newNode->setValueCategory(Expr::RValue);
  return newNode;
}
auto TypeChecker::promoteBoolToInt(Expr *expr) -> Expr * {
  assert(expr);
  assert(expr->getType());
  assert(expr->getType()->getKind() == Type::TK_BOOL);

  auto newType = ctx.getIntType();
  auto newNode = ctx.create<ImplicitCastExpr>(expr, expr->getLocation());
  newNode->setCastType(CastType::IntegralToFloat);
  newNode->setType(newType);
  newNode->setValueCategory(Expr::RValue);
  return newNode;
}

} // namespace mccomp
