#include <fmt/core.h>
#include <fstream>
#include <unordered_set>
#include <vector>

#define ALLOWED_INCLUDE
#include "error/clang_error.h"
#undef ALLOWED_INCLUDE

namespace {

const std::unordered_set<std::string> keywords = {
    "extern", "if", "else", "while", "return", "void", "int", "float", "bool"};

constexpr auto severity_color(mccomp::ClangErrorSeverity s) -> const char * {
  switch (s) {
  case mccomp::ClangErrorSeverity::NOTE:
    return mccomp::text_colors::CYAN;
  case mccomp::ClangErrorSeverity::WARNING:
    return mccomp::text_colors::YELLOW;
  case mccomp::ClangErrorSeverity::ERROR:
    return mccomp::text_colors::RED;
  }
  return mccomp::text_colors::RESET;
}

auto highlight_keywords(
    const std::string &line,
    const std::optional<std::pair<std::size_t, std::size_t>> &errorRange)
    -> std::string {
  std::string out;
  out.reserve(line.size());

  std::vector<std::size_t> map(line.size() + 1);

  std::size_t out_i = 0;

  // hoooorrrible miniature lexing logic
  for (std::size_t i = 0; i < line.size();) {
    map[i] = out_i;

    if (std::isalpha(line[i]) || line[i] == '_') {
      std::size_t start = i;
      while (i < line.size() && (std::isalnum(line[i]) || line[i] == '_'))
        ++i;
      std::string token = line.substr(start, i - start);

      if (keywords.count(token)) {
        out += mccomp::text_colors::BLUE;
        out += token;
        out += mccomp::text_colors::RESET;
        out_i += token.size() + std::strlen(mccomp::text_colors::BLUE) +
                 std::strlen(mccomp::text_colors::RESET);
      } else {
        out += token;
        out_i += token.size();
      }

      for (std::size_t j = start; j < i; ++j)
        // we need to map because the indexing of our words changes
        // after highlighting
        map[j] = out_i;
    } else {
      out += line[i];
      ++out_i;
      ++i;
    }
  }
  map[line.size()] = out_i;

  if (!errorRange)
    return out;

  auto [left, right] = *errorRange;
  if (left >= right || left == 0 || left > line.size() ||
      right > line.size() + 1)
    return out;

  std::size_t l = left - 1;
  std::size_t r = right - 1;

  std::size_t out_l = map[l];
  std::size_t out_r = map[r];

  return out.substr(0, out_l) + mccomp::text_colors::GREEN +
         out.substr(out_l, out_r - out_l) + mccomp::text_colors::RESET +
         out.substr(out_r);
}

} // namespace

namespace mccomp {

ClangError::ClangError(ClangErrorSeverity severity, std::string_view fileName,
                       std::size_t lineNo, std::size_t columnNo,
                       std::string &&message)
    : lineNo(lineNo), columnNo(columnNo), fileName(std::string(fileName)),
      severity(severity), message(std::move(message)), errorRange() {}

ClangError::ClangError(ClangErrorSeverity severity, std::string_view fileName,
                       std::size_t lineNo, std::size_t columnNo,
                       std::pair<std::size_t, std::size_t> errorRange,
                       std::string &&message)
    : lineNo(lineNo), columnNo(columnNo), fileName(std::string(fileName)),
      severity(severity), message(std::move(message)), errorRange(errorRange) {}

auto ClangError::to_string() const noexcept -> std::string {
  auto out = fmt::format("{}{}:{}:{}: {}{}: {}{}{}\n", text_colors::BOLD,
                         fileName, lineNo, columnNo, severity_color(severity),
                         mccomp::to_string(severity), text_colors::RESET,
                         text_colors::BOLD, message, text_colors::RESET);

  auto line = this->violatingLine();
  auto coloredLine = highlight_keywords(line, errorRange);
  out += fmt::format(" {:>4} | {}\n", this->lineNo, coloredLine);

  std::size_t caretPos = (columnNo > 0 ? columnNo : 1);
  if (caretPos > line.size())
    caretPos = line.size() > 0 ? line.size() : 1;

  std::string underline(line.size(), ' ');

  if (errorRange.has_value()) {
    auto [left, right] = *errorRange;

    if (left < right && left > 0 && right <= line.size()) {
      for (std::size_t i = left - 1; i < right - 1; ++i)
        underline[i] = '~';
    }
  }

  if (caretPos - 1 < underline.size())
    underline[caretPos - 1] = '^';

  out += fmt::format("      | {}{}{}\n", text_colors::GREEN, underline,
                     text_colors::RESET);

  return out;
}

auto ClangError::violatingLine() const -> std::string {
  auto inFile = std::ifstream(fileName);
  std::string line;
  if (inFile.good()) {
    for (std::size_t i = 1; i <= lineNo && std::getline(inFile, line); ++i) {
      if (i == lineNo)
        break;
    }
  }

  return line;
}

auto to_string(const ClangError &e) -> std::string { return e.to_string(); }

} // namespace mccomp
