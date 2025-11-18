#pragma once

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

  auto getArrayType(Type *elementType, const std::vector<std::size_t> &dims)
      -> Type * {
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

private:
  llvm::BumpPtrAllocator Alloc;

  BuiltinType *intType = nullptr;
  BuiltinType *floatType = nullptr;
  BuiltinType *boolType = nullptr;

  std::unordered_map<ArrayType, ArrayType *, ArrayTypeHash> arrayCache;
};
} // namespace mccomp
