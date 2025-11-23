#include <colors/colors.h>
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

static const char *invalid_sloc = "<invalid sloc>";

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
    const std::optional<std::tuple<std::size_t, std::size_t, std::size_t>>
        &errorRange) -> std::string {
  std::string out;
  out.reserve(line.size());

  std::vector<std::size_t> map(line.size() + 1);

  std::size_t out_i = 0;

  // hoooorrrible miniature lexing logic
  for (std::size_t i = 0; i < line.size();) {
    map[i] = out_i;

    if (std::isdigit(line[i])) {
      std::size_t start = i;
      while (i < line.size() && std::isdigit(line[i]))
        i++;
      std::string token = line.substr(start, i - start);

      out += mccomp::text_colors::GREEN;
      out += token;
      out += mccomp::text_colors::RESET;
      out_i += token.size() + std::strlen(mccomp::text_colors::GREEN) +
               std::strlen(mccomp::text_colors::RESET);

      for (std::size_t j = start; j < i; ++j)
        map[j] = out_i;
    } else if (std::isalpha(line[i]) || line[i] == '_') {
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

  auto [_, left, right] = *errorRange;
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
      severity(severity), message(std::move(message)), errorRange(),
      replacementSuggestion() {}

ClangError::ClangError(
    ClangErrorSeverity severity, std::string_view fileName, std::size_t lineNo,
    std::size_t columnNo,
    std::tuple<std::size_t, std::size_t, std::size_t> errorRange,
    std::string &&message)
    : lineNo(lineNo), columnNo(columnNo), fileName(std::string(fileName)),
      severity(severity), message(std::move(message)), errorRange(errorRange),
      replacementSuggestion() {}

ClangError::ClangError(ClangErrorSeverity severity, std::string_view fileName,
                       std::size_t lineNo, std::size_t columnNo,
                       std::string replacementSuggestion, std::string &&message)
    : lineNo(lineNo), columnNo(columnNo), fileName(std::string(fileName)),
      severity(severity), message(std::move(message)), errorRange(),
      replacementSuggestion(replacementSuggestion) {}

ClangError::ClangError(
    ClangErrorSeverity severity, std::string_view fileName, std::size_t lineNo,
    std::size_t columnNo, std::string replacementSuggestion,
    std::tuple<std::size_t, std::size_t, std::size_t> errorRange,
    std::string &&message)
    : lineNo(lineNo), columnNo(columnNo), fileName(std::string(fileName)),
      severity(severity), message(std::move(message)), errorRange(errorRange),
      replacementSuggestion(std::move(replacementSuggestion)) {}

ClangError::ClangError(ClangErrorSeverity severity, std::string &&message)
    : lineNo(0), columnNo(0), fileName(invalid_sloc), severity(severity),
      message(std::move(message)), errorRange(), replacementSuggestion() {}

auto ClangError::to_string() const noexcept -> std::string {
  std::string out = formatErrorHeader();

  if (fileName == invalid_sloc) {
    return out;
  }

  auto lines = this->violatingLines();
  if (lines.second) {
    out += formatMultiLineError(lines.first, *lines.second);
  } else {
    out += formatSingleLineError(lines.first);
  }

  for (const auto &err : this->attached) {
    out += err.to_string();
  }

  return out;
}

auto ClangError::formatErrorHeader() const noexcept -> std::string {
  return fmt::format("{}{}:{}:{}: {}{}: {}{}{}\n", text_colors::BOLD, fileName,
                     lineNo, columnNo, severity_color(severity),
                     mccomp::to_string(severity), text_colors::RESET,
                     text_colors::BOLD, message, text_colors::RESET);
}

auto ClangError::formatMultiLineError(const std::string &line1,
                                      const std::string &line2) const noexcept
    -> std::string {
  auto [otherLineNo, left, right] = *errorRange;

  // Ensure correct ordering of lines
  auto [primaryLineNo, secondaryLineNo, primaryLine, secondaryLine] =
      orderLines(lineNo, otherLineNo, line1, line2);

  std::string out = formatErrorLine(primaryLineNo, primaryLine);
  out += formatLineConnector(primaryLineNo, secondaryLineNo);
  out += formatErrorLine(secondaryLineNo, secondaryLine);
  out += formatMultiLineUnderline(secondaryLine, right);

  return out;
}

auto ClangError::orderLines(std::size_t line1No, std::size_t line2No,
                            const std::string &line1,
                            const std::string &line2) const noexcept
    -> std::tuple<std::size_t, std::size_t, std::string, std::string> {
  if (line2No < line1No) {
    return {line2No, line1No, line2, line1};
  }
  return {line1No, line2No, line1, line2};
}

auto ClangError::formatLineConnector(std::size_t fromLine,
                                     std::size_t toLine) const noexcept
    -> std::string {
  std::string connector;
  for (std::size_t i = fromLine + 1; i < toLine; ++i) {
    connector += fmt::format(" {:>4} | {}\n", "", "|");
  }
  return connector;
}

auto ClangError::formatErrorLine(std::size_t lineNumber,
                                 const std::string &line) const noexcept
    -> std::string {
  auto coloredLine = highlight_keywords(line, std::nullopt);
  return fmt::format(" {:>4} | {}\n", lineNumber, coloredLine);
}

auto ClangError::formatMultiLineUnderline(const std::string &line,
                                          std::size_t rightPos) const noexcept
    -> std::string {
  std::size_t lineLength = line.size();
  if (lineLength == 0) {
    return "";
  }

  std::string underline = createUnderlineString(lineLength, 0, rightPos);
  underline = addCaretToUnderline(underline, columnNo, lineLength);

  auto out = fmt::format("      | {}{}{}\n", text_colors::GREEN, underline,
                         text_colors::RESET);

  if (replacementSuggestion.has_value()) {
    out += formatReplacementSuggestion(*replacementSuggestion, columnNo);
  }

  return out;
}

auto ClangError::formatSingleLineError(const std::string &line) const noexcept
    -> std::string {
  auto coloredLine = highlight_keywords(line, errorRange);
  std::string out = fmt::format(" {:>4} | {}\n", this->lineNo, coloredLine);
  out += formatSingleLineUnderline(line);
  return out;
}

auto ClangError::formatSingleLineUnderline(
    const std::string &line) const noexcept -> std::string {
  std::string underline(line.size() + 1, ' ');

  if (errorRange.has_value()) {
    auto [lineNo, left, right] = *errorRange;
    underline = createUnderlineString(line.size() + 1, left, right);
  }

  underline = addCaretToUnderline(underline, columnNo, line.size());
  auto out = fmt::format("      | {}{}{}\n", text_colors::GREEN, underline,
                         text_colors::RESET);

  if (replacementSuggestion.has_value()) {
    out += formatReplacementSuggestion(*replacementSuggestion, columnNo);
  }

  return out;
}

auto ClangError::createUnderlineString(std::size_t length, std::size_t left,
                                       std::size_t right) const noexcept
    -> std::string {
  std::string underline(length, ' ');

  if (right > 0 && right <= length + 1) {
    // For multi-line case, underline from start to right position
    if (left == 0) {
      for (std::size_t i = 0; i < right - 1 && i < underline.size(); ++i) {
        underline[i] = '~';
      }
    }
    // For single-line case with valid range
    else if (left < right && left > 0 && right <= length) {
      for (std::size_t i = left - 1; i < right - 1 && i < underline.size();
           ++i) {
        underline[i] = '~';
      }
    }
  }

  return underline;
}

auto ClangError::addCaretToUnderline(std::string underline,
                                     std::size_t caretColumn,
                                     std::size_t lineLength) const noexcept
    -> std::string {
  std::size_t caretPos = normalizeCaretPosition(caretColumn, lineLength);

  if (caretPos - 1 < underline.size()) {
    underline[caretPos - 1] = '^';
  }

  return underline;
}

auto ClangError::normalizeCaretPosition(std::size_t column,
                                        std::size_t lineLength) const noexcept
    -> std::size_t {
  std::size_t caretPos = (column > 0 ? column : 1);
  if (caretPos > lineLength) {
    caretPos = lineLength > 0 ? lineLength + 1 : 1;
  }
  return caretPos;
}

auto ClangError::formatReplacementSuggestion(
    const std::string &suggestion, std::size_t caretColumn) const noexcept
    -> std::string {
  auto padding = std::string(std::max(static_cast<int>(caretColumn) -
                                          static_cast<int>(suggestion.size()),
                                      0),
                             ' ');
  return fmt::format("      | {}{}{}{}\n", text_colors::GREEN, padding,
                     suggestion, text_colors::RESET);
}

auto ClangError::violatingLines() const
    -> std::pair<std::string, std::optional<std::string>> {
  auto inFile = std::ifstream(fileName);
  std::string currLine;
  std::string line;
  std::optional<std::string> otherLine;

  auto other = std::get<0>(this->errorRange.value_or(std::tuple(0, 0, 0)));

  if (inFile.good()) {
    for (std::size_t i = 1;
         i <= std::max(lineNo, other) && std::getline(inFile, currLine); ++i) {
      if (i == lineNo) {
        line = std::move(currLine);
      } else if (i != lineNo && i == other) {
        otherLine = std::move(currLine);
      }
    }
  }

  return {line, otherLine};
}

auto to_string(const ClangError &e) -> std::string { return e.to_string(); }

} // namespace mccomp
