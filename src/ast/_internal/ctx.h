#include <ast/_internal/type.h>
#include <llvm/Support/Allocator.h>

namespace mccomp {
class ASTContext {
public:
  ~ASTContext() = default;
  ASTContext() {
    intType = create<BuiltinType>(Type::TK_INT);
    floatType = create<BuiltinType>(Type::TK_FLOAT);
    boolType = create<BuiltinType>(Type::TK_BOOL);
  }

  template <typename T, typename... Args> T *create(Args &&...args) {
    void *mem = Alloc.Allocate(sizeof(T), alignof(T));
    return new (mem) T(std::forward<Args>(args)...);
  }

  void *allocateRaw(std::size_t bytes,
                    std::size_t alignment = alignof(std::max_align_t)) {
    return Alloc.Allocate(bytes, alignment);
  }

  Type *getIntType() const { return intType; }
  Type *getFloatType() const { return floatType; }
  Type *getBoolType() const { return boolType; }

  Type *getArrayType(Type *elementType, const std::vector<std::size_t> &dims) {
    assert(elementType);
    assert(!dims.empty() && dims.size() <= 3);

    auto k = elementType->getKind();
    assert(k == Type::TK_INT || k == Type::TK_FLOAT);

    auto key = ArrayType{elementType, dims};
    auto it = arrayCache.find(key);
    if (it != arrayCache.end())
      return it->second;

    ArrayType *arr = create<ArrayType>(elementType, dims);
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
