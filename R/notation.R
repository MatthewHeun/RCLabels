#' Row and column notation
#'
#' @description
#' It is often convenient to represent matrix row and column names
#' with notation that includes a prefix and a suffix,
#' with corresponding separators or start-end string sequences.
#' There are several functions to generate specialized versions
#' or otherwise manipulate row and column names on their own or as row or column names.
#'
#' * `flip_pref_suff()` Switches the location of prefix and suffix, such that the prefix becomes the suffix, and
#'                      the suffix becomes the prefix.
#'                      E.g., "a -> b" becomes "b -> a" or "a \[b\]" becomes "b \[a\]".
#' * `get_pref_suff()` Selects only prefix or suffix, discarding notational elements
#'                     and the rejected part.
#'                     Internally, this function calls `split_pref_suff()` and selects only the desired portion.
#' * `notation_vec()` Builds a vector of notation symbols in a standard format.
#'                    By default, it builds a list of notation symbols that provides an arrow
#'                    separator (" -> ") between prefix and suffix.
#' * `paste_pref_suff()` `paste0`'s prefixes and suffixes, the inverse of `split_pref_suff()`.
#'                       Always returns a character vector.
#' * `preposition_notation()` Builds a list of notation symbols that provides (by default) square brackets around the suffix with a preposition ("prefix \[preposition suffix\]").
#' * `split_pref_suff()` Splits prefixes from suffixes, returning each in a list with names `pref` and `suff`.
#'                       If no prefix or suffix delimiters are found, `x` is returned in the `pref` item, unmodified,
#'                       and the `suff` item is returned as `""` (an empty string).
#'                       If there is no prefix, and empty string is returned for the `pref` item.
#'                       If there is no suffix, and empty string is returned for the `suff` item.
#' * `switch_notation()` Switches from one type of notation to another based on the `from` and `to` arguments.
#'                       Optionally, prefix and suffix can be `flip`ped.
#'
#' Parts of a `notation` vector are
#' "pref_start", "pref_end", "suff_start", and "suff_end".
#' None of the strings in a notation vector are considered part of the prefix or suffix.
#' E.g., "a -> b" in arrow notation means that "a" is the prefix and "b" is the suffix.
#' If `sep` only is specified for `notation_vec()` (default is " -> "),
#' `pref_start`, `pref_end`, `suff_start`, and `suff_end` are
#' set appropriately.
#'
#' For functions where the `notation` argument is used to identify portions of the row or column label
#' (such as `split_pref_suff()`, `get_pref_suff()`,
#' and the `from` argument to `switch_notation()`),
#' (Note: `flip_pref_suff()` cannot infer notation, because it switches prefix and suffix in a known, single notation.)
#' if `notation` is a list, it is treated as a store from which
#' the most appropriate notation is inferred by `infer_notation(choose_most_specific = TRUE)`.
#' Because default is `RCLabels::notations_list`,
#' notation is inferred by default.
#' The argument `choose_most_specific` tells what to do when two `notation`s match a label:
#' if `TRUE` (the default), the notation with most characters is selected.
#' If `FALSE`, the first matching notation in `notation` will be selected.
#' See details at `infer_notation()`.
#'
#' If specifying more than one `notation`, be sure the notations are in a list.
#' `notation = c(RCLabels::bracket_notation, RCLabels::arrow_notation)`
#' is unlikely to produce the desired result, because the notations
#' are concatenated together to form a long string vector.
#' Rather say
#' `notation = list(RCLabels::bracket_notation, RCLabels::arrow_notation)`.
#'
#' For functions that construct labels (such as `paste_pref_suff()`),
#' `notation` can be a list of notations
#' over which the paste tasks is mapped.
#' If `notation` is a list, it must have as many items as
#' there are prefix/suffix pairs to be pasted.
#'
#' If either `pref` or `suff` are a zero-length character vector
#' (essentially an empty character vector
#' such as obtained from `character()`)
#' input to `paste_pref_suff()`,
#' an error is thrown.
#' Instead, use an empty character string
#' (such as obtained from `""`).
#'
#' @param sep A string separator between prefix and suffix. Default is " -> ".
#' @param pref_start A string indicating the start of a prefix. Default is `NULL`.
#' @param pref_end A string indicating the end of a prefix. Default is the value of `sep`.
#' @param preposition A string that specifies a preposition for a notation.
#' @param suff_start A string indicating the start of a suffix. Default is the value of `sep`.
#' @param suff_end A string indicating the end of a suffix. Default is `NULL`.
#' @param x A string or vector of strings to be operated upon.
#' @param pref A string or list of strings that are prefixes. Default is `NULL`.
#' @param suff A string of list of strings that are suffixes. Default is `NULL`.
#' @param which Tells which to keep, the prefix ("pref") or the suffix ("suff").
#' @param ps A list of prefixes and suffixes in which each item of the list is itself a list with two items named `pref` and `suff`.
#' @param notation A notation vector generated by one of the `*_notation()` functions, such as
#'                 `notation_vec()`, `arrow_notation`, or `bracket_notation`.
#' @param inf_notation A boolean that tells whether to infer notation for `x`.
#'                     Default is `TRUE`.
#'                     See `infer_notation()` for details.
#' @param choose_most_specific A boolean that tells whether to choose the most specific
#'                             notation from the `notation` argument when the `notation` argument is a list.
#' @param transpose A boolean that tells whether to `purr::transpose()` the result.
#'                  Set `transpose = TRUE` when using `split_pref_suff()` in a `dplyr::mutate()`
#'                  call in the context of a data frame.
#'                  Default is `FALSE`.
#' @param from The `notation` to switch _away from_.
#' @param to The `notation` to switch _to_.
#' @param flip A boolean that tells whether to also flip the notation. Default is `FALSE`.
#' @param preposition A string used to indicate position for energy flows, typically "from" or "to" in different notations.
#' @param squish A boolean that tells whether to remove extra spaces in the output of `paste_*()` functions.
#'               Default is `TRUE`.
#'
#' @return For `notation_vec()`, `arrow_notation`, and `bracket_notation`,
#'           a string vector with named items `pref_start`, `pref_end`, `suff_start`, and `suff_end`;
#'         For `split_pref_suff()`, a string list with named items `pref` and `suff`.
#'         For `paste_pref_suff()`, `split_pref_suff()`, and `switch_notation()`,
#'           a string list in notation format specified by various `notation` arguments, including
#'           `from`, and `to`.
#'         For `keep_pref_suff`, one of the prefix or suffix or a list of prefixes or suffixes.
#'
#' @examples
#' notation_vec()
#' arrow_notation
#' bracket_notation
#' split_pref_suff("a -> b", notation = arrow_notation)
#' # Or infer the notation (by default from notations_list)
#' split_pref_suff("a -> b")
#' split_pref_suff(c("a -> b", "c -> d", "e -> f"))
#' split_pref_suff(c("a -> b", "c -> d", "e -> f"), transpose = TRUE)
#' flip_pref_suff("a [b]", notation = bracket_notation)
#' # Infer notation
#' flip_pref_suff("a [b]")
#' get_pref_suff("a -> b", which = "suff")
#' switch_notation("a -> b", from = arrow_notation, to = bracket_notation)
#' # Infer notation and flip prefix and suffix
#' switch_notation("a -> b", to = bracket_notation, flip = TRUE)
#' # Also works for vectors
#' switch_notation(c("a -> b", "c -> d"),
#'                 from = arrow_notation,
#'                 to = bracket_notation)
#' # Functions can infer the correct notation and return multiple matches
#' infer_notation("a [to b]",
#'                allow_multiple = TRUE,
#'                choose_most_specific = FALSE)
#' # Or choose the most specific notation
#' infer_notation("a [to b]",
#'                allow_multiple = TRUE,
#'                choose_most_specific = TRUE)
#' # When setting the from notation, only that type of notation will be switched
#' switch_notation(c("a -> b", "c [to d]"),
#'                 from = arrow_notation,
#'                 to = bracket_notation)
#' # But if notations are inferred, all notations can be switched
#' switch_notation(c("a -> b", "c [to d]"), to = bracket_notation)
#' # A double-switch can be accomplished.
#' # In this first example, `RCLabels::first_dot_notation` is inferred.
#' switch_notation("a.b.c", to = arrow_notation)
#' # In this second example,
#' # it is easier to specify the `from` and `to` notations.
#' switch_notation("a.b.c", to = arrow_notation) %>%
#'   switch_notation(from = first_dot_notation, to = arrow_notation)
#' # "" can be used as an input
#' paste_pref_suff(pref = "a", suff = "", notation = RCLabels::from_notation)
#' @name row-col-notation
NULL


#' @export
#' @rdname row-col-notation
notation_vec <- function(sep = " -> ",
                         pref_start = "", pref_end = "",
                         suff_start = "", suff_end = "") {
  if (all(nchar(c(pref_start, pref_end, suff_start, suff_end)) == 0) & nchar(sep) != 0) {
    return(c(pref_start = "", pref_end = sep,
             suff_start = sep, suff_end = ""))
  }
  c(pref_start = pref_start,
    pref_end = pref_end,
    suff_start = suff_start,
    suff_end = suff_end)
}


#' @export
#' @rdname row-col-notation
preposition_notation <- function(preposition, suff_start = " [", suff_end = "]") {
  notation_vec(sep = "",
               pref_start = "",
               pref_end = paste0(suff_start, preposition, " "),
               suff_start = paste0(suff_start, preposition, " "),
               suff_end = suff_end)
}


#' @export
#' @rdname row-col-notation
split_pref_suff <- function(x,
                            transpose = FALSE,
                            inf_notation = TRUE,
                            notation = RCLabels::notations_list,
                            choose_most_specific = TRUE) {
  # Any notation list is treated as a store from which appropriate notations are selected.
  notation <- infer_notation(x,
                             inf_notation = inf_notation,
                             notations = notation,
                             choose_most_specific = choose_most_specific,
                             retain_names = FALSE,
                             must_succeed = FALSE)

  if (length(x) > 1 & !is.list(notation)) {
    # We have an incoming vector of x with length > 1,
    # but notation is not a list.
    # This means that we don't have enough notations to line up with the number of labels in x.
    # So we need to replicate notation for as many labels as we have.
    notation <- make_list(notation, n = length(x), lenx = 1)
  }
  # Strip off first pref_start
  no_pref_start <- strip_label_part(x, notation = notation, part = "pref_start", pattern_pref = "^")

  # Strip off everything from first pref_end to end of string to obtain the prefix
  pref <- strip_label_part(no_pref_start, notation = notation, part = "pref_end", pattern_suff = ".*$")

  # Strip off pref from no_pref_start
  no_pref <- mapply(pref, no_pref_start, FUN = function(p, npstart) {
    gsub(pattern = paste0("^", Hmisc::escapeRegex(p)), replacement = "", x = npstart)
  }) %>%
    unname()

  # Strip off prefix end
  no_pref_end <- strip_label_part(no_pref, notation = notation, part = "pref_end", pattern_pref = "^")

  # Strip off suffix start
  no_suff_start <- strip_label_part(no_pref_end, notation = notation, part = "suff_start", pattern_pref = "^")

  # Strip off suffix end
  suff <- strip_label_part(no_suff_start, notation = notation, part = "suff_end", pattern_suff = "{1}$")

  # Return a list with the prefixes and suffixes in two members of a list
  out <- list(pref = pref, suff = suff)
  # Unless the caller wants to transpose the result,
  # especially if the result will go in a data frame.
  if (transpose) {
    out <- out %>%
      purrr::transpose()
  }
  return(out)
}


#' A convenience function to help splitting prefixes and suffixes
#'
#' This function should only ever see a single label (`x`)
#' and a single `notation`.
#'
#' If `notation` is `NULL`, `x` is returned, unmodified.
#'
#' @param x The label(s) to be split.
#' @param notation The notations to be used for each `x`.
#' @param part The part of the label to work on, such as "pref_start", "pref_end", "suff_start", or "suff_end".
#' @param pattern_pref The prefix to a regex pattern to be used in `gsub()`.
#' @param pattern_suff The suffix to a regex pattern to be used in `gsub()`.
#'
#' @return A label shorn of the part to be stripped.
strip_label_part <- function(x, notation, part, pattern_pref = "", pattern_suff = "") {
  # Take care of the case where notation is NULL,
  # likely because it could not be inferred.
  if (is.null(notation)) {
    return(x)
  }
  if (length(x) == 1) {
    if (notation[[part]] == "") {
      out <- x
    } else {
      out <- gsub(pattern = paste0(pattern_pref, Hmisc::escapeRegex(notation[[part]]), pattern_suff), replacement = "", x = x)
    }
  } else {
    # Vectorize over x and notation
    # Double-check that x and notation are the same length.
    # If not, this is an error.
    if (length(x) != length(notation)) {
      stop("length(x) and legth(notation) must be same length in strip_label_part()")
    }
    out <- vector(mode = "character", length = length(x))
    for (i in 1:length(x)) {
      if (is.null(notation[[i]])) {
        out[[i]] <- x[[i]]
      } else if (notation[[i]][[part]] == "") {
        out[[i]] <- x[[i]]
      } else {
        out[[i]] <- gsub(pattern = paste0(pattern_pref, Hmisc::escapeRegex(notation[[i]][[part]]), pattern_suff), replacement = "", x = x[[i]])
      }
    }
  }
  return(out)
}


#' @export
#' @rdname row-col-notation
paste_pref_suff <- function(ps = list(pref = pref, suff = suff),
                            pref = NULL,
                            suff = NULL,
                            notation = RCLabels::arrow_notation,
                            squish = TRUE) {

  single_paste_func <- function(this_ps, this_notation) {
    out <- paste0(this_notation[["pref_start"]], this_ps[["pref"]], this_notation[["pref_end"]])
    if (this_notation[["pref_end"]] != this_notation[["suff_start"]]) {
      out <- paste0(out, this_notation[["suff_start"]])
    }
    paste0(out, this_ps[["suff"]], this_notation[["suff_end"]])
  }

  # Check the lengths of prefix and suffix match
  len_pref_suff <- length(ps[["pref"]])

  if (len_pref_suff == 0) {
    # Try to transpose to see if we get the form that we need.
    transposed_ps <- purrr::transpose(ps)
    len_transposed_ps <- length(transposed_ps[["pref"]])
    if (len_transposed_ps > 0) {
      # We found a good structure here.
      len_pref_suff <- len_transposed_ps
      ps <- transposed_ps
    }
  }

  if (len_pref_suff != length(ps[["suff"]])) {
    # Try to recover by recycling the one that has length 1
    if (length(ps[["pref"]]) == 1) {
      ps[["pref"]] <- make_list(ps[["pref"]], n = length(ps[["suff"]])) %>% unlist()
    } else if (length(ps[["suff"]]) == 1) {
      ps[["suff"]] <- make_list(ps[["suff"]], n = length(ps[["pref"]])) %>% unlist()
    } else {
      stop(paste0("lengths of pref (", len_pref_suff, ") and ",
                  "suff (", length(ps[["suff"]]),
                  ") must match or one must be 1 so it can be recycled."))
    }
  }
  if (is.list(notation)) {
    len_notation <- length(notation)
  } else if (is.vector(notation)) {
    len_notation <- 1
  } else {
    stop("notation must be a list or a vector in paste_pref_suff()")
  }

  # At this point, we need to double-check the shape of both
  # ps and notation.

  # First, notation.
  if (len_pref_suff > 1 & len_notation == 1) {
    # If notation is a single-item list,
    # it should be replicated to be as long as the number of prefix/suffix combinations.
    notation <- make_list(notation, n = len_pref_suff, lenx = 1)
  }
  if (len_pref_suff == 1 & !is.list(notation)) {
    # Notation is coming in as a bare notation vector.
    # Make it a list so we can Map over it.
    notation <- make_list(notation, n = 1, lenx = 1)
  }
  if (!is.null(names(ps))){
    if (length(ps) == 2 & all(names(ps) == c("pref", "suff")) & !is.list(ps)) {
      # We have a single vector of the form ps - c(pref = xxxx, suff = yyyy)
      # Turn it into a list.
      ps <- list(list(pref = ps[["pref"]], suff = ps[["suff"]])) %>%
        purrr::transpose()
    }
  }

  # If pref or suff have names, the transpose call messes things up.
  ps <- list(pref = unname(ps[["pref"]]), suff = unname(ps[["suff"]]))

  ps <- purrr::transpose(ps)

  if (length(ps) == 0) {
    # Avoid going through the squish function,
    # which errors on older versions of R.
    # return(list())
    return(character())
  }

  out <- Map(f = single_paste_func, ps, notation) %>%
    unlist()

  if (squish) {
    out <- stringr::str_squish(out)
  }
  return(out)
}


#' @export
#' @rdname row-col-notation
# flip_pref_suff <- function(x, notation) {
flip_pref_suff <- function(x,
                           notation = RCLabels::notations_list,
                           inf_notation = TRUE,
                           choose_most_specific = TRUE) {
  notation <- infer_notation(x,
                             inf_notation = inf_notation,
                             notations = notation,
                             allow_multiple = FALSE,
                             retain_names = FALSE,
                             must_succeed = TRUE,
                             choose_most_specific = choose_most_specific)
  # Split prefixes and suffixes
  # Already inferred notation (or not),
  # so no need to infer again.
  pref_suff <- split_pref_suff(x, inf_notation = FALSE, notation = notation)
  paste_pref_suff(pref = pref_suff[["suff"]],
                  suff = pref_suff[["pref"]],
                  notation = notation) %>%
    as.character()
}


#' @export
#' @rdname row-col-notation
get_pref_suff <- function(x,
                          which = c("pref", "suff"),
                          inf_notation = TRUE,
                          notation = RCLabels::notations_list,
                          choose_most_specific = TRUE) {
  if (is.null(x)) {
    return(NULL)
  }
  which <- match.arg(which)
  split_pref_suff(x,
                  inf_notation = inf_notation,
                  notation = notation,
                  choose_most_specific = choose_most_specific) %>%
    magrittr::extract2(which) %>%
    as.character() %>%
    magrittr::set_names(rep(which, times = length(x)))
}


#' @export
#' @rdname row-col-notation
switch_notation <- function(x, from = RCLabels::notations_list, to, flip = FALSE, inf_notation = TRUE) {
  switch_func <- function(x) {
    ps <- split_pref_suff(x, inf_notation = inf_notation, notation = from)
    if (ps[["suff"]] == "") {
      # No split occurred, meaning the notation for prefix and suffix wasn't found.
      # In this case, return the string unmodified.
      return(x)
    }
    if (flip) {
      ps <- list(pref = ps[["suff"]], suff = ps[["pref"]])
    }
    paste_pref_suff(ps, notation = to)
  }

  if (length(x) > 1) {
    return(lapply(x, FUN = switch_func))
  }
  switch_func(x)
}


#' Infer the notation(s) for a row or column label
#'
#' It is convenient to know which notation is applicable to row or column labels.
#' This function infers which `notations` are appropriate for `x`.
#'
#' This function is vectorized.
#' Thus, `x` can be a vector, in which case the output is a list of notations.
#'
#' `notations` is treated as a store from which matches for each label in `x`
#' can be determined.
#' `notations` should be a named list of notations.
#' When `retain_names = TRUE`, the names on `notations` will be retained,
#' and the return value is _always_ a list.
#'
#' By default (`allow_multiple = FALSE`),
#' a single notation object is returned for each item in `x`
#' if only one notation in `notations`
#' is appropriate for `x`.
#' If `allow_multiple = FALSE` (the default) and more than one `notation` is applicable to `x`,
#' an error is thrown.
#' Multiple matches can be returned when `allow_multiple = TRUE`.
#'
#' If multiple notations are matched, the return value is a list.
#'
#' When `choose_most_specific = TRUE` (the default),
#' the most specific notation in `notations` is returned.
#' "Most specific" is defined as the matching notation
#' whose sum of characters in the `pref_start`, `pref_end`,
#' `suff_start` and `suff_end` elements
#' is greatest.
#' If `choose_most_specific = TRUE` and
#' two matching notations in `notations` have the same number of characters,
#' only the first match is returned.
#' When `choose_most_specific = TRUE`,
#' the value of `allow_multiple` no longer matters.
#' `allow_multiple = FALSE` is implied and
#' at most one of the `notations` will be returned.
#'
#' When `inf_notation = FALSE` (default is `TRUE`),
#' `notations` are returned unmodified,
#' essentially disabling this function.
#' Although calling with `inf_notation = FALSE` seems daft,
#' this behavior enables cleaner code elsewhere.
#'
#' @param x A row or column label (or vector of labels).
#' @param inf_notation A boolean that tells whether to infer notation for `x`.
#'                     Default is `TRUE`.
#' @param notations A list of notations from which matches will be inferred.
#'                  This function might not work as expected if
#'                  `notation` is not a list.
#'                  If `notation` is not a list,
#'                  `notations` is returned in full.
#'                  Default is `RCLabels::notations_list`.
#' @param allow_multiple A boolean that tells whether multiple notation matches
#'                       are allowed.
#'                       If `FALSE` (the default), multiple matches give an error.
#' @param retain_names A boolean that tells whether to retain names from `notations` on the
#'                     outgoing matches.
#'                     Default is `FALSE`.
#'                     If `TRUE`, the return value is _always_ a named list.
#'                     If only one of `notations` is returned
#'                     (for example, because `choose_most_specific = TRUE`),
#'                     names are never supplied.
#' @param choose_most_specific A boolean that indicates whether the most-specific notation
#'                             will be returned when more than one of `notations` matches `x`
#'                             and `allow_multiple = FALSE`.
#'                             When `FALSE`, the first matching notation in `notations`
#'                             is returned when `allow_multiple = FALSE`.
#'                             Default is `TRUE`.
#'                             See details.
#' @param must_succeed A boolean that if `TRUE` (the default),
#'                     causes an error to be thrown if a matching notation is not found
#'                     for any label in `x`.
#'                     When `FALSE`, an unsuccessful notation inference will return `NULL`.
#'
#' @return A single notation object (if `x` is a single row or column label)
#'         or a list of notation objects (if `x` is a vector or a list).
#'         If no `notations` match `x`, `NULL` is returned,
#'         either alone or in a list.
#'
#' @export
#'
#' @examples
#' # Does not match any notations in RCLabels::notations_list
#' # and throws an error, because the default value for `must_succeed`
#' # is `TRUE`.
#' \dontrun{
#' infer_notation("abc")
#' }
#' # This returns `NULL`, because `must_succeed = FALSE`.
#' infer_notation("abc", must_succeed = FALSE)
#' # This succeeds, because the label is in the form of a
#' # notation in `RCLabels::notation_list`,
#' # the default value of the `notation` argument.
#' infer_notation("a -> b")
#' # Names of the notations can be retained, in which case
#' # the return value is always a list.
#' infer_notation("a -> b", retain_names = TRUE)
#' # This function is vectorized.
#' # The list of labels matches
#' # all known notations in `RCLabels::notations_list`.
#' infer_notation(c("a -> b", "a (b)", "a [b]", "a [from b]", "a [of b]",
#'                  "a [to b]", "a [in b]", "a [-> b]", "a.b"),
#'                  retain_names = TRUE)
#' # By default, the most specific notation is returned.
#' # But when two or more matches are present,
#' # multiple notations can be returned, too.
#' infer_notation("a [from b]",
#'                allow_multiple = TRUE, retain_names = TRUE,
#'                choose_most_specific = FALSE)
#' infer_notation(c("a [from b]", "c [to d]"),
#'                allow_multiple = TRUE, retain_names = TRUE,
#'                choose_most_specific = FALSE)
#' # As shown above, "a \[from b\]" matches 2 notations:
#' # `RCLabels::bracket_notation` and `RCLabels::from_notation`.
#' # The default value for the notation argument is
#' # RCLabels::notations_list,
#' # which includes `RCLabels::bracket_notation`
#' # and `RCLabels::from_notation` in that order.
#' # Thus, there is some flexibility to how this function works
#' # if the value of the `notation` argument is a list of notations
#' # ordered from least specific to most specific,
#' # as `RCLabels::notations_list` is ordered.
#' # To review, the next call returns both `RCLabels::bracket_notation` and
#' # `RCLabels::from_notation`, because `allow_multiple = TRUE` and
#' # `choose_most_specific = FALSE`, neither of which are default.
#' infer_notation("a [from b]",
#'                allow_multiple = TRUE,
#'                choose_most_specific = FALSE,
#'                retain_names = TRUE)
#' # The next call returns `RCLabels::from_notation`, because
#' # the most specific notation is requested, and
#' # `RCLabels::from_notation` has more characters in its specification than
#' # `RCLabels::bracket_notation`.
#' infer_notation("a [from b]",
#'                choose_most_specific = TRUE,
#'                retain_names = TRUE)
#' # The next call returns the `RCLabels::bracket_notation`, because
#' # `choose_most_specific = FALSE`, and the first matching
#' # notation in `RCLabels::notations_list` is `RCLabels::bracket_notation`.
#' infer_notation("a [from b]",
#'                choose_most_specific = FALSE,
#'                retain_names = TRUE)
infer_notation <- function(x,
                           inf_notation = TRUE,
                           notations = RCLabels::notations_list,
                           allow_multiple = FALSE,
                           retain_names = FALSE,
                           choose_most_specific = TRUE,
                           must_succeed = TRUE) {
  if (!inf_notation) {
    return(notations)
  }

  if (length(x) == 1) {
    return(infer_notation_for_one_label(x,
                                        inf_notation = inf_notation,
                                        notations = notations,
                                        allow_multiple = allow_multiple,
                                        retain_names = retain_names,
                                        choose_most_specific = choose_most_specific,
                                        must_succeed = must_succeed))
  }
  # At this point, if x comes in with names, we don't want them on the output.
  # We only want names from notations on the output.
  # So eliminate the names on x.
  x <- unname(x)
  out <- lapply(x, FUN = function(one_label) {
    infer_notation_for_one_label(one_label,
                                 inf_notation = inf_notation,
                                 notations = notations,
                                 allow_multiple = allow_multiple,
                                 retain_names = retain_names,
                                 choose_most_specific = choose_most_specific,
                                 must_succeed = must_succeed)
  })
  if (all(lapply(out, length) == 1)) {
    return(unlist(out, recursive = FALSE))
  }
  return(out)
}


#' Infer the notation from one row or column label
#'
#' This is a non-public helper function for vectorized `infer_notation()`.
#'
#' @param x A single row or column label.
#' @param notations A list of notations from which matches will be inferred
#'                  This function might not work as expected if
#'                  `notation` is not a list.
#'                  If `notation` is not a list,
#'                  `notations` is returned in full.
#'                  Default is `RCLabels::notations_list`.
#' @param inf_notation A boolean that tells whether to infer notation for `x`.
#' @param allow_multiple A boolean that tells whether multiple notation matches
#'                       are allowed.
#'                       If `FALSE` (the default), multiple matches give an error.
#' @param retain_names A boolean that tells whether to retain names on the
#'                     outgoing matches.
#'                     Default is `FALSE`.
#'                     If `TRUE`, the return value is a named list.
#'                     If only one of `notations` is returned,
#'                     names are never supplied.
#' @param choose_most_specific A boolean that indicates if the most-specific notation
#'                             will be returned when more than one of `notations` matches `x`.
#'                             Default is `TRUE`.
#' @param must_succeed A boolean that if `TRUE` (the default),
#'                     causes an error to be thrown if a matching notation is not found
#'                     for any label in `x`.
#'                     When `FALSE`, an unsuccessful label inference will return `NULL`.
#'
#' @return A single matching notation object (if `allow_multiple = FALSE`, the default)
#'         or possibly multiple matching notation objects (if `allow_multiple = TRUE`).
#'         If no `notations` match `x`, `NULL`.
infer_notation_for_one_label <- function(x,
                                         inf_notation = TRUE,
                                         notations = RCLabels::notations_list,
                                         allow_multiple = FALSE,
                                         retain_names = FALSE,
                                         choose_most_specific = TRUE,
                                         must_succeed = TRUE) {
  if (!inf_notation | !is.list(notations) | length(notations) == 0) {
    return(notations)
  }
  notation_matches <- list()
  for (i in 1:length(notations)) {
    this_notation <- notations[[i]]
    locations <- lapply(this_notation, FUN = function(notation_element) {
      if (nchar(notation_element) == 0) {
        return(NULL)
      }
      gregexpr(pattern = notation_element, text = x, fixed = TRUE) %>%
        unlist()
    }) %>%
      # Eliminate all NULL entries
      purrr::discard(is.null)
    this_notation_match <- TRUE
    for (j in 1:(length(locations))) {
      # Check for appropriateness of the notation for string x.
      if (length(locations[[j]]) > 1) {
        # We have more than 1 match for this notation's delimiter, which is likely an error.
        # stop(paste0("More than 1 location in '", x, "' matched '",
        #             names(locations[j]), "' ('", this_notation[[names(locations[j])]],
        #             "') for ", names(notations[i]), " = ",
        #             paste0(notations[i])))

        # We have more than 1 match for this notation's delimiter.
        # Choose the first location.
        locations[[j]] <- locations[[j]][[1]]
      }
      if (locations[[j]] < 0) {
        # A notation is inappropriate for x if any location is < 0 (meaning the notation delimiter string was not found in x)
        this_notation_match <- FALSE
        break
      }
      if (j >= 2) {
        if (locations[[j]] < locations[[j-1]]) {
          # A notation is inappropriate for x if the location of any two consecutive notation elements are not in sequence.
          this_notation_match <- FALSE
          break
        }
      }
    }
    notation_matches <- notation_matches %>%
      append(this_notation_match)
  }
  notation_matches <- unlist(notation_matches)
  num_matches <- sum(notation_matches)
  the_matches_indices <- which(notation_matches, arr.ind = TRUE)
  the_matches <- notations[which(notation_matches)]
  if (num_matches == 0) {
    if (must_succeed) {
      stop(paste0("Unable to infer notation for '", x, "'"))
    }
    return(NULL)
  }
  if (num_matches == 1) {
    if (retain_names) {
      return(notations[the_matches_indices])
    } else {
      return(notations[[the_matches_indices]])
    }
  }
  # We have more than 1 match.
  if (choose_most_specific) {
    # If two or more notations match and choose_most_specific is true,
    # select the notation with the most characters.
    numchars <- lapply(the_matches, function(this_match) {
      nchar(this_match) %>% sum()
    })
    if (retain_names) {
      return(the_matches[which.max(numchars)])
    }
    return(the_matches[[which.max(numchars)]])
  }
  if (!allow_multiple) {
    # Choose the first match.
    out <- notations[the_matches_indices[[1]]]
    if (!retain_names) {
      out <- out %>%
        unname() %>%
        unlist()
    }
    return(out)
  }
  # if (!allow_multiple) {
  #   stop("multiple matches when allow_multiple = FALSE, choose_most_specific = FALSE, and must_succeed = FALSE in match_notation()")
  # }
  # At this point, allow_multiple = TRUE and choose_most_specific = FALSE.
  # Send back multiple notations.
  #
  # Without names
  if (!retain_names) {
    out <- notations[the_matches_indices] %>%
      unname()
    return(out)
  }
  # Or with names.
  # At this point,
  # allow_multiple = TRUE,
  # choose_most_specific = FALSE, and
  # retain_names = TRUE.
  notations[the_matches_indices]
}


