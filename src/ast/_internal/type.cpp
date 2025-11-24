#include <ast/_internal/type.h>

namespace mccomp {

auto Type::convertsTo(const Type *type, bool onFunctionReturn) const
    -> tl::expected<std::optional<CastType>, std::string> {
  auto from = this->getKind();
  auto to = type->getKind();
  if (to == Type::TK_VOID) {
    if (!onFunctionReturn)
      return tl::unexpected("bad case expecting void cast to");
    if (from == Type::TK_VOID)
      return std::nullopt;
    else
      return tl::unexpected("void function should not return a value");
  }

  if (from == Type::TK_VOID) {
    return tl::unexpected(onFunctionReturn
                              ? "non-void function should return a value"
                              : "cannot cast void to " + type->to_string());
  }

  if (to == Type::TK_ARRAY) {
    if (onFunctionReturn)
      return tl::unexpected("returning an array type is not supported");
    else
      return tl::unexpected(
          fmt::format("array type '{}' is not assignable", type->to_string()));
  }

  if (to == Type::TK_PTR) {
    if (onFunctionReturn)
      return tl::unexpected("returning a pointer type is not supported");

    if (this == type) {
      return std::nullopt;
    } else if (this->getKind() == Type::TK_PTR) {
      auto sameUnderlying =
          llvm::dyn_cast<PointerType>(this)->getElementType() ==
          llvm::dyn_cast<PointerType>(type)->getElementType();
      if (sameUnderlying) {
        return std::nullopt;
      } else {
        return tl::unexpected(fmt::format(
            "incompatible pointer types assigning to '{}' from '{}'",
            type->to_string(), this->to_string()));
      }
    } else {
      return tl::unexpected(fmt::format(
          "incompatible {} to pointer conversion assigning to '{}' from '{}'",
          this->to_string(), type->to_string(), this->to_string()));
    }
  }

  if (to == Type::TK_FUNCTION) {
    return tl::unexpected(fmt::format("non-object type '{}' is not assignable",
                                      type->to_string()));
  }

  static constexpr struct {
    Type::Kind from, to;
    std::optional<CastType> cast;
    const char *error;
  } conversions[] = {
      {Type::TK_INT, Type::TK_INT, std::nullopt, nullptr},
      {Type::TK_INT, Type::TK_FLOAT, CastType::IntegralToFloat, nullptr},
      {Type::TK_INT, Type::TK_BOOL, std::nullopt,
       "implicit (narrowing) conversion from int to bool"},
      {Type::TK_FLOAT, Type::TK_INT, std::nullopt,
       "implicit (narrowing) conversion from float to int"},
      {Type::TK_FLOAT, Type::TK_FLOAT, std::nullopt, nullptr},
      {Type::TK_FLOAT, Type::TK_BOOL, std::nullopt,
       "implicit (narrowing) conversion from float to bool"},
      {Type::TK_BOOL, Type::TK_BOOL, std::nullopt, nullptr},
      {Type::TK_BOOL, Type::TK_INT, CastType::BooleanToIntegral, nullptr},
      {Type::TK_BOOL, Type::TK_FLOAT, CastType::BooleanToFloat, nullptr},
      {Type::TK_FUNCTION, Type::TK_PTR, CastType::FunctionToPointerDecay,
       nullptr},
      {Type::TK_ARRAY, Type::TK_PTR, CastType::ArrayToPointerDecay, nullptr},
      {Type::TK_PTR, Type::TK_BOOL, std::nullopt,
       "implicit narrowing conversion from pointer to bool"},
  };

  for (const auto &conv : conversions) {
    if (conv.from == from && conv.to == to) {
      if (conv.error) {
        return tl::unexpected(conv.error);
      } else {
        return conv.cast;
      }
    }
  }

  return tl::unexpected(
      fmt::format("no implicit conversion available to '{}' from '{}'",
                  type->to_string(), this->to_string()));
}

} // namespace mccomp
