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
#' `pattern_type` controls the type of pattern created:
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
#' make_or_pattern(strings = c("a", "b"), pattern_type = "leading")
#' make_or_pattern(strings = c("a", "b"), pattern_type = "trailing")
#' make_or_pattern(strings = c("a", "b"), pattern_type = "anywhere")
#' make_or_pattern(strings = c("a", "b"), pattern_type = "literal")
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


#' Find or replace row or column labels that match a regular expression
#'
#' `match_by_pattern()` tells whether row or column labels
#' match a regular expression.
#' Internally, `grepl()` decides whether a match occurs.
#' `replace_by_pattern()` replaces portions of row of column labels
#' when a regular expression is matched.
#' Internally, `gsub()` performs the replacements.
#'
#' By default (`pieces = "all"`), complete labels (as strings) are checked for matches
#' and replacements.
#' If `pieces == "pref"` or `pieces == "suff"`,
#' only the prefix or the suffix is checked for matches and replacements.
#' Alternatively, `pieces = "noun"` or `pieces = <<preposition>>` indicate
#' that only specific pieces of labels are to be checked for matches and replacements.
#' When `pieces = <<preposition>>`, only the object of `<<preposition>>` is
#' checked for matches and replacement.
#'
#' `pieces` can be a vector, indicating multiple pieces to be checked for matches
#' and replacements.
#' But if any of the `pieces` are "all", all pieces are checked and replaced.
#' If `pieces` is "pref" or "suff", only one can be specified.
#'
#'
#' @param labels The row and column labels to be modified.
#' @param regex_pattern The regular expression pattern to determine matches and replacements.
#'                      Consider using `Hmisc::escapeRegex()` to escape `regex_pattern`
#'                      before calling this function.
#' @param replacement For `replace_by_pattern()`, the string that replaces
#'                    all matches to `regex_pattern`.
#' @param pieces The pieces of row or column labels to be checked for matches or replacements.
#'               See details.
#' @param prepositions A vector of strings that count as prepositions.
#'                     Default is `RCLabels::prepositions_list`.
#'                     Used to detect prepositional phrases
#'                     if `pieces` are to be interpreted as prepositions.
#' @param notation The notation used in `labels`.
#'                 Default is `RCLabels::bracket_notation`.
#' @param ... Other arguments passed to `grepl()` or `gsub()`,
#'            such as `ignore.case`, `perl`, `fixed`,
#'            or `useBytes`.
#'            See examples.
#'
#' @return A logical vector of same length as `labels`,
#'         where `TRUE` indicates a match was found and `FALSE` indicates otherwise.
#'
#' @examples
#' labels <- c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]")
#' # With default `pieces` argument, matching is done for whole labels.
#' match_by_pattern(labels, regex_pattern = "Production")
#' match_by_pattern(labels, regex_pattern = "Coal")
#' match_by_pattern(labels, regex_pattern = "USA")
#' # Check beginnings of labels
#' match_by_pattern(labels, regex_pattern = "^Production")
#' # Check at ends of labels: no match.
#' match_by_pattern(labels, regex_pattern = "Production$")
#' # Can match on nouns or prepositions.
#' match_by_pattern(labels, regex_pattern = "Production", pieces = "noun")
#' # Gives FALSE, because "Production" is a noun.
#' match_by_pattern(labels, regex_pattern = "Production", pieces = "in")
#' @name regex_funcs
NULL


#' @export
#' @rdname regex_funcs
match_by_pattern <- function(labels,
                             regex_pattern,
                             pieces = "all",
                             prepositions = RCLabels::prepositions_list,
                             notation = RCLabels::bracket_notation,
                             ...) {

  if ("all" %in% pieces) {
    return(grepl(pattern = regex_pattern, x = labels, ...))
  }

  if ("pref" %in% pieces | "suff" %in% pieces) {
    # Ensure the length is 1
    if (length(pieces) != 1) {
      stop(paste0('If pieces contains "pref" or "suff", its length must be 1. Length was ', length(pieces), '.'))
    }
    return(grepl(pattern = regex_pattern,
                 x = get_pref_suff(labels,
                                    which = pieces,
                                    notation = notation),
                 ...))
  }

  # At this point, treat pieces as specifying a noun or prepositions.
  keepers <- split_noun_pp(labels, prepositions = prepositions, notation = notation) %>%
    lapply(FUN = function(this_split_label) {
      this_split_label[pieces]
    })

  sapply(keepers, FUN = function(this_keeper) {
    grepl(pattern = regex_pattern,  x = this_keeper, ...) %>%
      # any() takes care of multiple pieces.
      any(na.rm = TRUE)
  })
}


#' @export
#' @rdname regex_funcs
replace_by_pattern <- function(labels,
                               regex_pattern,
                               replacement,
                               pieces = "all",
                               prepositions = RCLabels::prepositions_list,
                               notation = RCLabels::bracket_notation,
                               ...) {
  if ("all" %in% pieces) {
    return(gsub(pattern = regex_pattern,
                replacement = replacement,
                x = labels,
                ...))
  }

  if ("pref" %in% pieces | "suff" %in% pieces) {
    # Ensure the length is 1
    if (length(pieces) != 1) {
      stop(paste0('If pieces contains "pref" or "suff", its length must be 1. Length was ', length(pieces), '.'))
    }
    split <- split_pref_suff(labels, notation = notation)
    piece_for_replacement <- split[[pieces]]
    replaced_piece <- gsub(pattern = regex_pattern,
                           replacement = replacement,
                           x = piece_for_replacement,
                           ...)
    split[[pieces]] <- replaced_piece
    return(paste_pref_suff(split, notation = notation))
  }

  # At this point, treat pieces as specifying a noun or prepositions.
  splits <- split_noun_pp(labels, prepositions = prepositions, notation = notation)
  for (i_splits in 1:length(splits)) {
    this_split_label <- splits[[i_splits]]
    for (this_piece in pieces) {
      this_piece_for_replacement <- this_split_label[[this_piece]]
      replaced_piece <- gsub(pattern = regex_pattern,
                             replacement = replacement,
                             x = this_piece_for_replacement,
                             ...)
      this_split_label[[this_piece]] <- replaced_piece
    }
    splits[[i_splits]] <- this_split_label
  }
  paste_noun_pp(splits, notation = notation)
}


#' Make a list of items in x, regardless of x's type
#'
#' Repeats `x` as necessary to make `n` of them.
#' Does not try to simplify `x`.
#'
#' If `x` is itself a vector or list,
#' you may want to override the default.
#' For example, if `x` is a list that should be duplicated several times,
#' set `lenx = 1`.
#'
#' @param x The object to be duplicated.
#' @param n The number of times to be duplicated.
#' @param lenx The length of item `x`.
#'             Be default, `lenx` is taken to be `length(x)`,
#'
#' @return A list of `x` duplicated `n` times
#'
#' @export
#'
#' @examples
#' m <- matrix(c(1:6), nrow=3, dimnames = list(c("r1", "r2", "r3"), c("c2", "c1")))
#' make_list(m, n = 1)
#' make_list(m, n = 2)
#' make_list(m, n = 5)
#' make_list(list(c(1,2), c(1,2)), n = 4)
#' m <- matrix(1:4, nrow = 2)
#' l <- list(m, m+100)
#' make_list(l, n = 4)
#' make_list(l, n = 1) # Warning because l is trimmed.
#' make_list(l, n = 5) # Warning because length(l) (i.e., 2) not evenly divisible by 5
#' make_list(list(c("r10", "r11"), c("c10", "c11")), n = 2) # Confused by x being a list
#' make_list(list(c("r10", "r11"), c("c10", "c11")), n = 2, lenx = 1) # Fix by setting lenx = 1
make_list <- function(x, n, lenx = ifelse(is.vector(x), length(x), 1)){
  out <- vector(mode = "list", length = n)
  reptimes <- as.integer(n / lenx)
  if (n %% lenx != 0 & lenx != 1) {
    warning("n not evenly divisible by length(x)")
  }
  if (lenx == 1) {
    return(
      lapply(X = 1:n, FUN = function(i){
        out[[i]] <- x
      })
    )
  }
  if (n < lenx) {
    # Fewer items than length of x is desired
    return(x[[1:n]])
  }
  for (cycle in 1:reptimes) {
    for (xindex in 1:lenx) {
      outindex <- (cycle - 1)*lenx + (xindex)
      out[[outindex]] <- x[[xindex]]
    }
  }
  if (n %% length(x) == 0) {
    # Had an even number of cycles
    return(out)
  }
  for (outindex in (reptimes*lenx + 1):n) {
    xindex <- outindex - reptimes*lenx
    out[[outindex]] <- x[[xindex]]
  }
  return(out)
}
