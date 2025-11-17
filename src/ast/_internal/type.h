#include <cassert>
#include <cstdint>
#include <vector>

namespace mccomp {

class Type {
public:
  enum Kind {
    TK_INT,
    TK_FLOAT,
    TK_BOOL,
    TK_ARRAY,
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
    assert(k == TK_INT || k == TK_FLOAT || k == TK_BOOL);
  }
};

class ArrayType : public Type {
  Type *elementType;
  std::vector<std::size_t> dims;

public:
  // This asserts the underlying type is an int or float, according
  // to the specification.
  ArrayType(Type *element, std::vector<std::size_t> dims)
      : Type(TK_ARRAY), elementType(element), dims(std::move(dims)) {
    assert(element->getKind() == TK_INT || element->getKind() == TK_FLOAT);
    assert(this->dims.size() >= 1 && this->dims.size() <= 3);
  }

  auto getElementType() const -> Type * { return elementType; }
  auto getDimensions() const -> const std::vector<std::size_t> & {
    return dims;
  }
};

class ArrayTypeHash {
  auto operator()(const ArrayType &type) const noexcept -> std::size_t {
    auto hsh = std::hash<Type *>{}(type.getElementType());
    for (auto i = std::uint8_t(); i < type.getDimensions().size(); ++i) {
      hsh ^= std::hash<std::size_t>{}(type.getDimensions()[i] +
                                      0x9e3779b97f4a7c15ULL + (hsh << 6) +
                                      (hsh >> 2));
    }

    return hsh;
  }
};

} // namespace mccomp
