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


#' Convert a suffix to its prepositional phrases
#'
#' A suffix can consist of several prepositional phrases.
#' This function splits the prepositional phrases apart,
#' returning a list of those phrases.
#' Each prepositional phrase consists of a proposition
#' (see `RCLabels::prepositions`) and its object.
#'
#' Some labels will have no suffix and no prepositions.
#' When there are no prepositions, `NA` is returned.
#'
#' @param labels The row and column labels from which prepositional phrases are to be extracted.
#' @param notation The notation object that describes the labels.
#'                 Default is `RCLabels::bracket_notation`.
#'
#' @return A list of lists of prepositional phrases.
#'         The length of the list is the same as the length of `labels`.
#'
#' @export
#'
#' @examples
separate_pp <- function(labels,
                        notation = RCLabels::bracket_notation,
                        prepositions = RCLabels::prepositions) {
  if (length(labels) == 1) {
    out <- split_pref_suff(labels, notation) |>
      magrittr::extract2("suff") |>
      as.list()
  } else {
    out <- split_pref_suff(labels, notation) |>
      purrr::transpose() |>
      magrittr::extract2("suff")
  }
  # Find location of all prepositions
  preposition_words <- paste0(prepositions, " ")
  prep_patterns <- make_or_pattern(preposition_words,
                                   pattern_type = "anywhere")

  # Iterate over all labels


  # Recombine in a list

  # Return the list


  # start_locations <- gregexpr(prep_patterns, text = out)
  # start_locations_vec <- unlist(start_locations)
  # if (length(start_locations_vec) == 0) {
  #   start_lcoations <- NA_real_
  #   end_locations <- NA_real_
  # }
  # if (length(start_locations_vec) == 1) {
  #   end_locations <- length(labels)
  # } else {
  #   end_locations <- c(start_locations_vec[-1] - 1, lapply(labels, function(l) {length(l)}))
  # }
  # if (is.na(start_locations)) {
  #   return(NA_real_)
  # }
  # # Extract the strings for each prepositional phrase
  #

  return(out)

}


get_pp <- function(labels, notation = RCLabels::bracket_notation) {
  if (length(labels) == 1) {
    out <- split_pref_suff(labels, notation) |>
      magrittr::extract2("suff") |>
      as.list()
  } else {
    out <- split_pref_suff(labels, notation) |>
      purrr::transpose() |>
      magrittr::extract2("suff")
  }

  return(out)

}
