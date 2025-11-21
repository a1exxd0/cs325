#pragma once

#include <ast/_internal/expr_node.h>
#include <cassert>
#include <cstdint>
#include <vector>

namespace mccomp {

class Expr;

class Type {
public:
  enum Kind {
    TK_INT,
    TK_FLOAT,
    TK_BOOL,
    TK_VOID,
    TK_ARRAY,
    TK_PTR,
  };

private:
  Kind kind;

protected:
  explicit Type(Kind k) : kind(k) {}

public:
  virtual ~Type() = default;
  auto getKind() const -> Kind { return kind; }
};

class BuiltinType : public Type {
public:
  explicit BuiltinType(Kind k) : Type(k) {
    assert(k == TK_INT || k == TK_FLOAT || k == TK_BOOL || k == TK_VOID);
  }
};

class PointerType : public Type {
  Type *element;

public:
  explicit PointerType(Type *element) : Type(TK_PTR), element(element) {}

  auto getElementType() const -> Type * { return element; }
  auto operator==(const PointerType &other) const noexcept -> bool {
    return element == other.element;
  }
};

class ArrayType : public Type {
  Type *elementType;
  std::vector<Expr *> dims;

public:
  // This asserts the underlying type is an int or float, according
  // to the specification.
  ArrayType(Type *element, std::vector<Expr *> dims)
      : Type(TK_ARRAY), elementType(element), dims(std::move(dims)) {
    assert(element->getKind() == TK_INT || element->getKind() == TK_FLOAT);
    assert(this->dims.size() >= 1 && this->dims.size() <= 3);
  }

  auto getElementType() const -> Type * { return elementType; }
  auto getDimensions() const -> const std::vector<Expr *> & { return dims; }

  auto operator==(const ArrayType &other) const noexcept -> bool {
    auto sameUnderlying = this->elementType == other.elementType;
    auto sameDimSize = this->dims == other.dims;

    return sameUnderlying && sameDimSize;
  }
};

class ArrayTypeHash {
public:
  auto operator()(const ArrayType &type) const noexcept -> std::size_t {
    auto hsh = std::hash<Type *>{}(type.getElementType());
    for (auto i = std::uint8_t(); i < type.getDimensions().size(); ++i) {
      // TODO: check safety
      hsh ^= std::hash<long>{}(reinterpret_cast<long>(type.getDimensions()[i]) +
                               0x9e3779b + (hsh << 6) + (hsh >> 2));
    }

    return hsh;
  }
};

} // namespace mccomp
