#pragma once

#include <ast/_internal/expr_node.h>
#include <ast/_internal/type.h>
#include <llvm/Support/Allocator.h>

namespace mccomp {
class ASTContext {
public:
  ~ASTContext() = default;
  ASTContext() {
    intType = this->create<BuiltinType>(Type::TK_INT);
    floatType = this->create<BuiltinType>(Type::TK_FLOAT);
    boolType = this->create<BuiltinType>(Type::TK_BOOL);
    voidType = this->create<BuiltinType>(Type::TK_VOID);
  }

  template <typename T, typename... Args> auto create(Args &&...args) -> T * {
    auto mem = Alloc.Allocate(sizeof(T), alignof(T));
    return new (mem) T(std::forward<Args>(args)...);
  }

  auto allocateRaw(std::size_t bytes,
                   std::size_t alignment = alignof(std::max_align_t))
      -> void * {
    return Alloc.Allocate(bytes, alignment);
  }

  auto getIntType() const -> Type * { return intType; }
  auto getFloatType() const -> Type * { return floatType; }
  auto getBoolType() const -> Type * { return boolType; }
  auto getVoidType() const -> Type * { return voidType; }

  auto getArrayType(Type *elementType, const std::vector<Expr *> &dims)
      -> ArrayType * {
    assert(elementType);
    assert(!dims.empty() && dims.size() <= 3);

    auto k = elementType->getKind();
    assert(k == Type::TK_INT || k == Type::TK_FLOAT);

    auto key = ArrayType{elementType, dims};
    auto it = arrayCache.find(key);
    if (it != arrayCache.end())
      return it->second;

    auto arr = create<ArrayType>(elementType, dims);
    arrayCache.emplace(std::move(key), arr);
    return arr;
  }

  auto getPtrType(Type *elementType) -> PointerType * {
    assert(elementType);
    auto it = ptrCache.find(elementType);
    if (it != ptrCache.end()) {
      return it->second;
    }

    auto ptr = create<PointerType>(elementType);
    ptrCache.emplace(elementType, ptr);
    return ptr;
  }

  auto getFunctionType(Type *returnType, const std::vector<Type *> argTypes)
      -> FunctionType * {
    assert(returnType);
    auto key = FunctionType{returnType, argTypes};
    auto it = functionCache.find(key);
    if (it != functionCache.end())
      return it->second;

    auto fun = create<FunctionType>(returnType, argTypes);
    functionCache.emplace(std::move(key), fun);
    return fun;
  }

private:
  llvm::BumpPtrAllocator Alloc;

  BuiltinType *intType = nullptr;
  BuiltinType *floatType = nullptr;
  BuiltinType *voidType = nullptr;
  BuiltinType *boolType = nullptr;

  std::unordered_map<ArrayType, ArrayType *, ArrayTypeHash> arrayCache;
  std::unordered_map<Type *, PointerType *> ptrCache;
  std::unordered_map<FunctionType, FunctionType *, FunctionTypeHash>
      functionCache;
};
} // namespace mccomp
