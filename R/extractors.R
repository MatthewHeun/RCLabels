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


#' Extract prepositional phrases of row and column labels
#'
#' This function extracts the suffix of a row or column label as a
#' single string.
#'
#' @param labels A list or vector of labels from which nouns are to be extracted.
#' @param notation The notation type to be used when extracting nouns.
#'                 Default is `RCLabels::bracket_notation`.
#' @param prepositions A list of prepositions, used to detect prepositional phrases.
#'                     Default is `RCLabels::prepositions`.
#'
#' @return All prepositional phrases in a suffix.
#'
#' @export
#'
#' @examples
#' get_pps(c("a [in b]", "c [of d]"))
#' get_pps(c("a [of b in c]", "d [-> e of f]"))
get_pps <- function(labels,
                    notation = RCLabels::bracket_notation,
                    prepositions = RCLabels::prepositions) {
  suffixes <- keep_pref_suff(labels, keep = "suff", notation = notation)
  # Location prepositions
  preposition_words <- paste0(prepositions, " ")
  prep_patterns <- make_or_pattern(preposition_words,
                                   pattern_type = "anywhere")
  start_locations <- gregexpr(prep_patterns, text = suffixes) |>
    unlist()
  if (length(start_locations) == 0) {
    start_locations <- NA_real_
    end_locations <- NA_real_
  } else if (length(start_locations) == 1) {
    end_locations <- nchar(suffixes)
  } else {
    end_locations <- c(start_locations[-1] - 2, nchar(suffixes))
  }
  substring(suffixes,
            first = start_locations[[1]],
            last = end_locations[[length(end_locations)]])
}


#' Extract prepositions from row and column labels
#'
#' This function extracts prepositions from a list of row and column labels.
#' The list has outer structure of the number of labels and
#' an inner structure of each prepositional phrase in the specific label.
#'
#' @param labels The row and column labels from which prepositional phrases are to be extracted.
#' @param notation The notation object that describes the labels.
#'                 Default is `RCLabels::bracket_notation`.
#' @param prepositions A vector of strings to be treated as prepositions.
#'                     Note that a space is appended to each word internally,
#'                     so, e.g., "to" becomes "to ".
#'                     Default is `RCLabels::prepositions`.
#'
#' @return A list of prepositions.
#'
#' @export
#'
#' @examples
#' get_preps(c("a [of b into c]", "d [-> e of f]"))
get_preps <- function(labels,
                      notation = RCLabels::bracket_notation,
                      prepositions = RCLabels::prepositions) {
  pps <- keep_pref_suff(labels, keep = "suff", notation = notation)
  preposition_words <- paste0(prepositions, " ")
  prep_patterns <- make_or_pattern(preposition_words,
                                   pattern_type = "anywhere")

  start_locations <- gregexpr(prep_patterns, text = pps)
  out <- list()
  for (i_pp in 1:length(pps)) {
    pp <- pps[[i_pp]]
    sl <- start_locations[[i_pp]]
    out_this_pp <- list()
    for (i_prep in 1:length(sl)) {
      this_prep <- substring(pp,
                             first = sl[[i_prep]],
                             # -2 accounts for
                             # the starting character and
                             # the space.
                             last = sl[[i_prep]] + attr(sl, which = "match.length")[[i_prep]] - 2)
      out_this_pp <- append(out_this_pp, this_prep)
    }
    out <- append(out, list(as.character(out_this_pp)))
  }
  return(out)
}


#' Extract objects of prepositional phrases in row and column labels
#'
#' This function extracts the objects of prepositional phrases
#' from row and column labels.
#' The format of the output is a list of
#' named items, one name for each preposition encountered in labels.
#' Objects are `NA` if there is no prepositional phrase starting
#' with that preposition.
#'
#' @param labels The row and column labels from which prepositional phrases are to be extracted.
#' @param notation The notation object that describes the labels.
#'                 Default is `RCLabels::bracket_notation`.
#' @param prepositions A vector of strings to be treated as prepositions.
#'                     Note that a space is appended to each word internally,
#'                     so, e.g., "to" becomes "to ".
#'                     Default is `RCLabels::prepositions`.
#'
#' @return A list of objects of prepositional phrases,
#'         with names being prepositions, and values being objects.
#'
#' @export
#'
#' @examples
#' get_objects("a [of b into c]", "d [of Coal from e -> f]")
get_objects <- function(labels,
                        notation = RCLabels::bracket_notation,
                        prepositions = RCLabels::prepositions) {
  # pps <- keep_pref_suff(labels, keep = "suff", notation = notation)
  # if (!inherits(pps, what = "list")) {
  #   pps <- list(pps)
  # }
  # preps <- get_preps(labels, notation = notation, prepositions = prepositions)
  # mapply(pps, preps, FUN = function(this_pp, these_preps) {
  #   sapply(these_preps, FUN = function(this_prep) {
  #     this_prep_plus_space <- paste0(this_prep, " ")
  #     gsub(pattern = this_prep_plus_space, replacement = "", x = this_pp)
  #   })
  # })

  pps <- keep_pref_suff(labels, keep = "suff", notation = notation)
  preposition_words <- paste0(prepositions, " ")
  prep_patterns <- make_or_pattern(preposition_words,
                                   pattern_type = "anywhere")

  prep_start_locations <- gregexpr(prep_patterns, text = pps)
  prep_lengths <- lapply(prep_start_locations, FUN = function(psl) {
    attr(psl, which = "match.length")
  })


  prep_end_locations <- mapply(prep_start_locations, prep_lengths, SIMPLIFY = FALSE, FUN = function(psl, pl) {
    mapply(psl, pl, FUN = function(this_psl, this_pl) {
      this_psl + this_pl - 2
    })
  })

  # Figure out the object start and end locations.
  obj_start_locations <- lapply(prep_end_locations, function(pel) {
    pel + 2
  })

  obj_end_locations <- mapply(prep_start_locations, pps, SIMPLIFY = FALSE, FUN = function(this_psl, this_pp) {
    out <- this_psl[-1] - 2
    out[[length(out) + 1]] <- nchar(this_pp)
    return(out)
  })

  mapply(pps, obj_start_locations, obj_end_locations, SIMPLIFY = FALSE, FUN = function(this_pp, these_osls, these_oels) {
    mapply(these_osls, these_oels, FUN = function(osl, oel) {
      substring(this_pp, first = osl, last = oel)
    })
  })



  # prep_end_locations <- lapply(prep_start_locations, FUN = function(psl) {
  #   psl
  # })
  # out <- list()
  # for (i_pp in 1:length(pps)) {
  #   pp <- pps[[i_pp]]
  #   sl <- start_locations[[i_pp]]
  #   out_this_pp <- list()
  #   for (i_prep in 1:length(sl)) {
  #     this_prep <- substring(pp,
  #                            first = sl[[i_prep]],
  #                            # -2 accounts for
  #                            # the starting character and
  #                            # the space.
  #                            last = sl[[i_prep]] + attr(sl, which = "match.length")[[i_prep]] - 2)
  #     out_this_pp <- append(out_this_pp, this_prep)
  #   }
  #   out <- append(out, list(as.character(out_this_pp)))
  # }
  # return(out)




}


#' Split row and column labels into nouns and prepositional phrases
#'
#' This function is similar to `split_pref_suff()` in that it returns a list.
#' However, this function's list is more detailed than
#' `split_pref_suff()`.
#' The return value from this function is a list
#' with the first named item being the prefix (with the name `noun`)
#' followed by objects of prepositional phrases
#' (with names being prepositions that precede the objects).
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
#'
#' @export
#'
#' @examples
#' split_labels(c("a [of b in c]", "d [of e into f]"),
#'              notation = bracket_notation)
split_labels <- function(labels,
                         notation = RCLabels::bracket_notation,
                         prepositions = RCLabels::prepositions) {


  # This function should return a named vector.
  # Names are:
  #  - noun
  #  - prepositions
  # Values are:
  #  - for noun: the noun
  #  - for prepositions: the object of the prepositions
  # E.g., "a [of b in c]" would return
  # c(noun = "a", of = "b", in = "c")

  nouns <- get_nouns(labels, notation = notation)
  pps <- get_pps(labels, notation = notation, prepositions = prepositions)
  out <- list(noun = nouns, pps = pps)
  if (length(labels) == 1) {
    return(out)
  }
  out |>
    # Make it a list so that we get 1 item where there are two,
    # thereby allowing transose() to work.
    purrr::modify_depth(.depth = -1, .f = list) |>
    purrr::transpose() |>
    # Now undo the list we just made.
    # It is now up one level.
    purrr::modify_depth(.depth = -2, .f = unlist)
}

