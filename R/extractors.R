#' Extract nouns from labels
#'
#' @param labels
#' @param notation
#'
#' @return The nouns from row and column labels.
#'
#' @export
#'
#' @examples
#' get_noun("a [b]", bracket_notation)
get_noun <- function(labels, notation) {
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



