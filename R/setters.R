#' Set nouns in labels
#'
#' This function sets the nouns of row and column labels.
#' The length of `new_nouns` must be the same as the length of `labels`.
#'
#' @param labels The labels in which the nouns will be set.
#' @param new_nouns The new nouns to be set in `labels`.
#'                  Must be same length as `labels`.
#' @param notation The notation used in `labels`.
#'                 Default is `RCLabels::bracket_notation`.
#'
#' @return A character vector of same length as labels
#'         with `new_nouns`.
#'
#' @export
#'
#' @examples
#' labels <- c("a [of b in c]", "d [of e in USA]")
#' set_nouns(labels, c("a_plus", "g"))
set_nouns <- function(labels, new_nouns, notation = RCLabels::bracket_notation) {
  num_labels <- length(labels)
  num_new_nouns <- length(new_nouns)
  if (num_labels != num_new_nouns) {
    stop("The number of labels must equal the number of new nouns in set_nouns()")
  }
  splitted <- split_labels(labels)
  mapply(splitted, new_nouns, SIMPLIFY = FALSE, FUN = function(this_split_label, this_new_noun) {
    this_split_label[["noun"]] <- this_new_noun
    this_split_label
  }) |>
    recombine_labels(notation = notation)
}
