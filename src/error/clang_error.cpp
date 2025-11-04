#include "clang_error.h"

namespace mccomp {

ClangError::ClangError(ClangErrorSeverity severity, std::string_view fileName,
                       std::size_t lineNo, std::size_t columnNo,
                       std::string &&message)
    : lineNo(lineNo), columnNo(columnNo), fileName(fileName),
      severity(severity), message(std::move(message)) {}

auto ClangError::to_string() const noexcept -> std::string {
  return fmt::format("{}:{}:{}: {}: {}", fileName, lineNo, columnNo,
                     mccomp::to_string(severity), message);
}

auto to_string(const ClangError &e) -> std::string { return e.to_string(); }

} // namespace mccomp
