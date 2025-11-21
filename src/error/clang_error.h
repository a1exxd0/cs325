#pragma once
#define FMT_HEADER_ONLY

#ifndef ALLOWED_INCLUDE
#error "Do not include this directly, use <error/error.h>"
#endif

#include <colors/colors.h>
#include <cstddef>
#include <fmt/compile.h>
#include <fmt/format.h>
#include <optional>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

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
  const std::optional<std::tuple<std::size_t, std::size_t, std::size_t>>
      errorRange;
  const std::optional<std::string> replacementSuggestion;

public:
  ClangError(ClangErrorSeverity severity, std::string &&message);

  ClangError(ClangErrorSeverity severity, std::string_view fileName,
             std::size_t lineNo, std::size_t columnNo, std::string &&message);

  ClangError(ClangErrorSeverity severity, std::string_view fileName,
             std::size_t lineNo, std::size_t columnNo,
             std::string replacementSuggestion, std::string &&message);

  ClangError(ClangErrorSeverity severity, std::string_view fileName,
             std::size_t lineNo, std::size_t columnNo,
             std::tuple<std::size_t, std::size_t, std::size_t> errorRange,
             std::string &&message);

  ClangError(ClangErrorSeverity severity, std::string_view fileName,
             std::size_t lineNo, std::size_t columnNo,
             std::string replacementSuggestion,
             std::tuple<std::size_t, std::size_t, std::size_t> errorRange,
             std::string &&message);

  auto to_string() const noexcept -> std::string;

  auto violatingLines() const
      -> std::pair<std::string, std::optional<std::string>>;

  friend auto operator<<(std::ostream &, const ClangError &) -> std::ostream &;

  std::vector<ClangError> attached;

private:
  auto formatErrorHeader() const noexcept -> std::string;
  auto formatMultiLineError(const std::string &line1,
                            const std::string &line2) const noexcept
      -> std::string;
  auto orderLines(std::size_t line1No, std::size_t line2No,
                  const std::string &line1,
                  const std::string &line2) const noexcept
      -> std::tuple<std::size_t, std::size_t, std::string, std::string>;
  auto formatLineConnector(std::size_t fromLine,
                           std::size_t toLine) const noexcept -> std::string;
  auto formatErrorLine(std::size_t lineNumber,
                       const std::string &line) const noexcept -> std::string;
  auto formatMultiLineUnderline(const std::string &line,
                                std::size_t rightPos) const noexcept
      -> std::string;
  auto formatSingleLineError(const std::string &line) const noexcept
      -> std::string;
  auto formatSingleLineUnderline(const std::string &line) const noexcept
      -> std::string;
  auto createUnderlineString(std::size_t length, std::size_t left,
                             std::size_t right) const noexcept -> std::string;
  auto addCaretToUnderline(std::string underline, std::size_t caretColumn,
                           std::size_t lineLength) const noexcept
      -> std::string;

  auto normalizeCaretPosition(std::size_t column,
                              std::size_t lineLength) const noexcept
      -> std::size_t;

  auto formatReplacementSuggestion(const std::string &suggestion,
                                   std::size_t caretCol) const noexcept
      -> std::string;
};

auto to_string(const ClangError &e) -> std::string;

inline auto operator<<(std::ostream &os, const ClangError &err)
    -> std::ostream & {
  auto str = fmt::format(FMT_STRING("severity: {}, {}:{}:{}, {}\n"),
                         to_string(err.severity), err.fileName, err.lineNo,
                         err.columnNo, err.message);
  if (err.errorRange) {
    const auto [lineNo, startCol, endCol] = err.errorRange.value();
    str += fmt::format("range: {}:{}:{}\n", lineNo, startCol, endCol);
  }

  if (err.replacementSuggestion) {
    str += fmt::format("suggestion: {}\n", err.replacementSuggestion.value());
  }

  os << str;
  if (err.attached.size()) {
    os << std::string("children: \n");
    for (const auto &child : err.attached) {
      os << child;
    }
    os << std::string("\n");
  }

  return os;
}

}; // namespace mccomp
