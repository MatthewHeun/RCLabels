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
#' get_nouns("a [b]", bracket_notation)
#' # Also works with vectors and lists.
#' get_nouns(c("a [b]", "c [d]"))
#' get_nouns(list("a [b]", "c [d]"))
get_nouns <- function(labels, notation = RCLabels::bracket_notation) {
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


#' Extract prepositional phrases from labels
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
#' get_pps(c("a [of b in c]", "d [of e into f]"), bracket_notation)
get_pps <- function(labels,
                    notation = RCLabels::bracket_notation,
                    prepositions = RCLabels::prepositions) {
  if (length(labels) == 1) {
    all_suffixes <- split_pref_suff(labels, notation) |>
      magrittr::extract2("suff") |>
      as.list()
  } else {
    all_suffixes <- split_pref_suff(labels, notation) |>
      purrr::transpose() |>
      magrittr::extract2("suff")
  }
  # Find location of all prepositions
  preposition_words <- paste0(prepositions, " ")
  prep_patterns <- make_or_pattern(preposition_words,
                                   pattern_type = "anywhere")

  # Iterate over all suffixes to find start and end locations
  starts_ends <- lapply(all_suffixes, function(suff) {
    start_locations <- gregexpr(prep_patterns, text = suff) |>
      unlist()
    if (length(start_locations) == 0) {
      start_locations <- NA_real_
      end_locations <- NA_real_
    } else if (length(start_locations) == 1) {
      end_locations <- nchar(suff)
    } else {
      end_locations <- c(start_locations[-1] - 2, nchar(suff))
    }
    return(list(start_location = start_locations,
                end_location = end_locations) |>
             purrr::transpose())
  })

  # Iterate over all suffixes to extract strings for prepositional phrases
  pps_in_suffixes <- list()
  for (i_suffixes in 1:length(all_suffixes)) {
    pps_in_suffix <- list()
    for (i_suff in 1:length(starts_ends[[i_suffixes]])) {
      pps_in_suffix <- append(pps_in_suffix,
                              substring(all_suffixes[[i_suffixes]],
                                        first = starts_ends[[i_suffixes]][[i_suff]][["start_location"]],
                                        last = starts_ends[[i_suffixes]][[i_suff]][["end_location"]]))
    }
    pps_in_suffixes <- append(pps_in_suffixes, list(pps_in_suffix))
  }

  return(pps_in_suffixes)

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
