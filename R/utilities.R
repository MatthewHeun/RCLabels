#' Create "or" regex patterns
#'
#' This function makes "or" regex patterns from vectors or lists of strings.
#' This function can be used with the `matsbyname::select_rows_byname()`
#' and `matsbyname::select_cols_byname` functions.
#' `make_or_pattern()` correctly escapes special characters in `strings`,
#' such as `(` and `)`, as needed.
#' Thus, it is highly recommended that `make_or_pattern` be used when
#' constructing patterns for row and column selections with
#' `matsbyname::select_rows_byname()` and `matsbyname::select_cols_byname()`.
#'
#' \code{pattern_type} controls the type of pattern created:
#' \itemize{
#'   \item{`exact` produces a regex pattern that selects row or column names by exact match.}
#'   \item{`leading` produces a regex pattern that selects row or column names if the item in `strings` matches
#'         the beginnings of row or column names.}
#'   \item{`trailing` produces a regex pattern that selects row or column names if the item in `strings` matches
#'         the ends of row or column names.}
#'   \item{`anywhere` produces a regex pattern that selects row or column names if the item in `strings` matches
#'         any substring of row or column names.}
#'   \item{`literal` returns `strings` unmodified, and it is up to the caller to formulate a correct regex.}
#' }
#'
#' @param strings A vector of row and column names.
#' @param pattern_type One of "exact", "leading", "trailing", "anywhere", or "literal". Default is "exact".
#'
#' @return An "or" regex pattern suitable for selecting row and column names.
#'         Amenable for use with `matsbyname::select_rows_byname` or `matsbyname::select_cols_byname`.
#'
#' @export
#'
#' @examples
#' make_or_pattern(strings = c("a", "b"), pattern_type = "exact")
make_or_pattern <- function(strings, pattern_type = c("exact", "leading", "trailing", "anywhere", "literal")){
  pattern_type <- match.arg(pattern_type)
  if (pattern_type == "literal") {
    return(strings)
  }
  out <- Hmisc::escapeRegex(strings)
  # Add leading caret if needed
  if (pattern_type %in% c("exact", "leading")) {
    out <- paste0("^", out)
  }
  # Add trailing dollar sign if needed
  if (pattern_type %in% c("exact", "trailing")) {
    out <- paste0(out, "$")
  }
  paste0(out, collapse = "|")
}


#' Tell whether row or column labels match a regular expression
#'
#' This function tells whether row or column labels
#' match a regular expression.
#' Internally, `grepl()` decides whether a match occurs.
#'
#' By default (`pieces = "all`), complete labels (as strings) are checked for matches.
#' If `pieces == "pref"` or `pieces == "suff"`,
#' only the prefix or the suffix is checked for matches.
#' Alternatively, `pieces = "noun"` or `pieces = <<preposition>>` indicate
#' that only specific pieces of labels are to be checked for matches.
#'
#' `pieces` can be a vector, indicating multiple pieces to be checked for matches.
#' But if any of the `pieces` are "all", all pieces are checked.
#' If `pieces` is "pref" or "suff", only one can be specified.
#'
#'
#' @param labels The row and column labels in which pieces will be modified.
#' @param regex_pattern The regular expression pattern to determine matches.
#'                      Consider using `Hmisc::escapeRegex()` to escape `regex_pattern`
#'                      before calling this function.
#' @param piece The piece (or pieces) of row or column labels to be checked for matches.
#'              See details.
#' @param notation The notation used in `labels`.
#'                 Default is `RCLabels::bracket_notation`.
#' @param ... Other arguments passed to `grepl()`, such as `ignore.case`, `perl`, `fixed`,
#'            or `useBytes`.
#'            See examples.
#'
#' @return A logical vector of same length as `labels`,
#'         where `TRUE` indicates a match was found and `FALSE` indicates otherwise.
#' @export
#'
#' @examples
match_by_pattern <- function(labels,
                          regex_pattern,
                          pieces = "all",
                          prepositions = RCLabels::prepositions,
                          notation = RCLabels::bracket_notation, ...) {

  if ("all" %in% pieces) {
    return(grepl(pattern = regex_pattern, x = labels, ...))
  }

  if ("pref" %in% pieces | "suff" %in% pieces) {
    # Ensure the length is 1
    if (length(pieces) != 1) {
      stop(paste0('If pieces contains "pref" or "suff", its length must be 1. Length was ', length(pieces), '.'))
    }
    if (pieces == "pref") {
      return(grepl(pattern = regex_pattern, x = keep_pref_suff(labels, keep = "pref", notation = notation), ...))
    }

    if (pieces == "suff") {
      return(grepl(pattern = regex_pattern, x = keep_pref_suff(labels, keep = "suff", notation = notation), ...))
    }
  }

  # At this point, treat pieces as specifying a noun or prepositions.
  keepers <- split_labels(labels, prepositions = prepositions, notation = notation) |>
    lapply(FUN = function(this_split_label) {
      this_split_label[pieces]
    })

  sapply(keepers, FUN = function(this_keeper) {
    grepl(pattern = regex_pattern,  x = this_keeper, ...) |>
      # any() takes care of multiple pieces.
      any(na.rm = TRUE)
  })
}






















