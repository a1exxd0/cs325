#define FMT_HEADER_ONLY

#include <cstddef>
#include <fmt/core.h>
#include <string>
#include <string_view>

namespace mccomp {

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

public:
  ClangError(ClangErrorSeverity severity, std::string_view fileName,
             std::size_t lineNo, std::size_t columnNo, std::string &&message);

  auto to_string() const noexcept -> std::string;
};

auto to_string(const ClangError &e) -> std::string;

}; // namespace mccomp
