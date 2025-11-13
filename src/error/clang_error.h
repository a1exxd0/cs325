#pragma once
#define FMT_HEADER_ONLY

#ifndef ALLOWED_INCLUDE
#error "Do not include this directly, use <error/error.h>"
#endif

#include <cstddef>
#include <optional>
#include <string>
#include <string_view>

namespace mccomp {

namespace text_colors {
constexpr const char *BOLD = "\033[1m";
constexpr const char *RED = "\033[31m";
constexpr const char *YELLOW = "\033[33m";
constexpr const char *RESET = "\033[0m";
constexpr const char *GREEN = "\033[32;1m";
constexpr const char *BLUE = "\033[34;1m";
constexpr const char *CYAN = "\033[1;96m";
}; // namespace text_colors

enum class ClangErrorSeverity {
  NOTE,
  WARNING,
  ERROR,
};

constexpr inline auto to_string(ClangErrorSeverity s) noexcept -> const char * {
  switch (s) {
  case ClangErrorSeverity::NOTE:
    return "note";
  case ClangErrorSeverity::WARNING:
    return "warning";
  case ClangErrorSeverity::ERROR:
    return "error";
  }
  return "<unknown>";
}

class ClangError {
  const std::size_t lineNo;
  const std::size_t columnNo;
  const std::string fileName;
  const ClangErrorSeverity severity;
  const std::string message;
  const std::optional<std::pair<std::size_t, std::size_t>> errorRange;

public:
  ClangError(ClangErrorSeverity severity, std::string_view fileName,
             std::size_t lineNo, std::size_t columnNo, std::string &&message);

  ClangError(ClangErrorSeverity severity, std::string_view fileName,
             std::size_t lineNo, std::size_t columnNo,
             std::pair<std::size_t, std::size_t> errorRange,
             std::string &&message);

  auto to_string() const noexcept -> std::string;

  auto violatingLine() const -> std::string;
};

auto to_string(const ClangError &e) -> std::string;

}; // namespace mccomp
