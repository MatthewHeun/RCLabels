#' Extract nouns from labels
#'
#' Nouns are the first part of a row-column label,
#' "a" in "a \[b\]".
#'
#' @param labels A list or vector of labels from which nouns are to be extracted.
#' @param notation The notation type to be used when extracting nouns.
#'                 Default is `RCLabels::bracket_notation`.
#'
#' @return A list of nouns from row and column labels.
#'
#' @export
#'
#' @examples
#' get_noun("a [b]", bracket_notation)
#' # Also works with vectors and lists.
#' get_noun(c("a [b]", "c [d]"))
#' get_noun(list("a [b]", "c [d]"))
get_noun <- function(labels, notation = RCLabels::bracket_notation) {
  if (length(labels) == 1) {
    out <- split_pref_suff(labels, notation) |>
      magrittr::extract2("pref") |>
      as.list()
  } else {
    out <- split_pref_suff(labels, notation) |>
      purrr::transpose() |>
      magrittr::extract2("pref")
  }
  return(out)
}



get_pp <- function(labels, notation = RCLabels::bracket_notation) {

}
