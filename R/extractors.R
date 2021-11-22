#' Extract nouns from labels
#'
#' Nouns are the first part of a row-column label,
#' "a" in "a \[b\]".
#' Internally, this function calls `keep_pref_suff()`
#' and asks for the prefix.
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
  keep_pref_suff(labels, keep = "pref", notation = notation)
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
#' @param prepositions A vector of strings to be treated as prepositions.
#'                     Note that a space is appended to each word internally,
#'                     so, e.g., "to" becomes "to ".
#'                     Default is `RCLabels::prepositions`.
#'
#' @return A list of lists of prepositional phrases.
#'         The structure of the returned list is the same as the length of `labels`
#'         with an additional level added
#'         in which each item is one prepositional phrase.
#'
#' @export
#'
#' @examples
#' get_pps(c("a [of b in c]", "d [of e into f]"), bracket_notation)
get_pps <- function(labels,
                    notation = RCLabels::bracket_notation,
                    prepositions = RCLabels::prepositions) {

  # Get all suffixes, which are assumed to be prepositional phrases.
  preposition_words <- paste0(prepositions, " ")
  prep_patterns <- make_or_pattern(preposition_words,
                                   pattern_type = "anywhere")

  suffixes <- keep_pref_suff(labels, keep = "suff", notation = notation) |>
    purrr::modify_depth(.depth = -1, function(this_suff) {
      start_locations <- gregexpr(prep_patterns, text = this_suff) |>
        unlist()
      if (length(start_locations) == 0) {
        start_locations <- NA_real_
        end_locations <- NA_real_
      } else if (length(start_locations) == 1) {
        end_locations <- nchar(this_suff)
      } else {
        end_locations <- c(start_locations[-1] - 2, nchar(this_suff))
      }

      mapply(start_locations, end_locations, FUN = function(start_loc, end_loc) {
        substring(this_suff, first = start_loc, last = end_loc)
      }) |>
        # Make a list out of the resulting vector,
        # thereby ensuring 1 item replaces the string.
        list()
    })
  if (length(suffixes) == 1) {
    return(unlist(suffixes))
  }
  suffixes |>
    # Eliminate the list we just added.
    purrr::modify_depth(.depth = -2, unlist)
}


#' Split row and column labels into nouns and prepositional phrases
#'
#' @param labels The row and column labels from which prepositional phrases are to be extracted.
#' @param notation The notation object that describes the labels.
#'                 Default is `RCLabels::bracket_notation`.
#' @param prepositions A vector of strings to be treated as prepositions.
#'                     Note that a space is appended to each word internally,
#'                     so, e.g., "to" becomes "to ".
#'                     Default is `RCLabels::prepositions`.
#'
#' @return A list of lists with items named `noun` and `pp`.
#' @export
#'
#' @examples
split_labels <- function(labels,
                         notation = RCLabels::bracket_notation,
                         prepositions = RCLabels::prepositions) {
  nouns <- get_nouns(labels, notation = notation)
  pps <- get_pps(labels, notation = notation, prepositions = prepositions)
  list(noun = nouns, pp = pps) |>
    purrr::transpose()
}

