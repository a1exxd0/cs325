#pragma once
#include "error/error.h"
#include <ast/ast.h>
#include <tl/expected.hpp>

namespace util {
[[noreturn]] inline auto todo_impl(const char *file, int line) -> void {
  fmt::println("todo reached at {}:{}", file, line);
  std::abort();
}

template <class T> [[noreturn]] auto todo(const char *file, int line) -> T {
  todo_impl(file, line);
}

inline auto badParseCase(const char *file = __builtin_FILE(),
                         const char *func_name = __builtin_FUNCTION(),
                         int line = __builtin_LINE(),
                         int col = __builtin_COLUMN())
    -> tl::unexpected<mccomp::ClangError> {
  return tl::unexpected(mccomp::ClangError(
      mccomp::ClangErrorSeverity::ERROR, file, line, col,
      fmt::format("unexpected parse case. check lookahead for function {}",
                  func_name)));
}

template <typename V, typename... Args>
inline auto allocateNode(mccomp::ASTContext &ctx, Args &&...args)
    -> tl::expected<V *, mccomp::ClangError> {
  auto node = ctx.create<V>(std::forward<Args>(args)...);
  if (node == nullptr) {
    auto error = mccomp::ClangError(mccomp::ClangErrorSeverity::ERROR,
                                    "ran out of space to allocate ast node");

    return tl::unexpected(std::move(error));
  }

  return node;
}

using mccomp::TokenType;
static constexpr auto FIRST_local_decls = std::array<TokenType, 3>{
    TokenType::INT_TOK, TokenType::FLOAT_TOK, TokenType::BOOL_TOK};
static constexpr auto FIRST_local_decl = FIRST_local_decls;
static constexpr auto FIRST_stmt_list = std::array<TokenType, 12>{
    TokenType::IDENT,   TokenType::MINUS,     TokenType::NOT,
    TokenType::INT_LIT, TokenType::FLOAT_LIT, TokenType::BOOL_LIT,
    TokenType::SC,      TokenType::LBRA,      TokenType::IF,
    TokenType::WHILE,   TokenType::RETURN};
static constexpr auto FIRST_stmt = FIRST_stmt_list;
static constexpr auto FIRST_expr_stmt = std::array<TokenType, 8>{
    TokenType::IDENT,    TokenType::MINUS,   TokenType::NOT,
    TokenType::LPAR,     TokenType::INT_LIT, TokenType::FLOAT_LIT,
    TokenType::BOOL_LIT, TokenType::SC};
static constexpr auto FIRST_block = std::array<TokenType, 1>{TokenType::LBRA};
static constexpr auto FIRST_if_stmt = std::array<TokenType, 1>{TokenType::IF};
static constexpr auto FIRST_while_stmt =
    std::array<TokenType, 1>{TokenType::WHILE};
static constexpr auto FIRST_return_stmt =
    std::array<TokenType, 1>{TokenType::RETURN};
static constexpr auto FIRST_expr = std::array<TokenType, 7>{
    TokenType::IDENT,   TokenType::MINUS,     TokenType::LPAR,
    TokenType::INT_LIT, TokenType::FLOAT_LIT, TokenType::BOOL_LIT};
static constexpr auto FIRST_else_stmt =
    std::array<TokenType, 1>{TokenType::ELSE};
static constexpr auto FIRST_lvalue = std::array<TokenType, 1>{TokenType::IDENT};
static constexpr auto FIRST_unary_expr = std::array<TokenType, 7>{
    TokenType::MINUS,   TokenType::NOT,     TokenType::LPAR,
    TokenType::IDENT,   TokenType::INT_LIT, TokenType::FLOAT_LIT,
    TokenType::BOOL_LIT};
static constexpr auto FIRST_mul_expr = FIRST_unary_expr;
static constexpr auto FIRST_add_expr = FIRST_mul_expr;
static constexpr auto FIRST_rel_expr = FIRST_add_expr;
static constexpr auto FIRST_eq_expr = FIRST_rel_expr;
static constexpr auto FIRST_and_expr = FIRST_eq_expr;
static constexpr auto FIRST_or_expr = FIRST_and_expr;
static constexpr auto FIRST_primary = std::array<TokenType, 5>{
    TokenType::LPAR, TokenType::IDENT, TokenType::INT_LIT, TokenType::FLOAT_LIT,
    TokenType::BOOL_LIT};
static constexpr auto FIRST_arg_list = FIRST_expr;
static constexpr auto FOLLOW_args = std::array<TokenType, 1>{TokenType::RPAR};

} // namespace util
