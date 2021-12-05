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
  keep_pref_suff(labels, keep = "pref", notation = notation) |>
    magrittr::set_names(rep("noun", length(labels)))
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
  if (length(start_locations) == 1) {
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
#' get_prepositions(c("a [of b into c]", "d [-> e of f]"))
get_prepositions <- function(labels,
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
#' get_objects(c("a [of b into c]", "d [of Coal from e -> f]"))
get_objects <- function(labels,
                        notation = RCLabels::bracket_notation,
                        prepositions = RCLabels::prepositions) {

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

  prepositions <- get_prepositions(labels, notation = notation, prepositions = prepositions)

  mapply(pps, obj_start_locations, obj_end_locations, prepositions,
                    SIMPLIFY = FALSE, USE.NAMES = FALSE, FUN = function(this_pp, these_osls, these_oels, these_pps) {
    mapply(these_osls, these_oels, these_pps, FUN = function(osl, oel, these_preps) {
      substring(this_pp, first = osl, last = oel) |>
        magrittr::set_names(these_preps)
    })
  })
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
#' Unlike `split_pref_suff()`, it does not make sense to have a `transpose`
#' argument on `split_labels()`.
#' Labels may not have the same structure,
#' e.g., they may have different prepositions.
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

  nouns <- get_nouns(labels, notation = notation) |>
    as.list() |>
    unname() |>
    lapply(FUN = function(this_noun) {
      magrittr::set_names(this_noun, "noun")
    })
  objects <- get_objects(labels, notation = notation, prepositions = prepositions)

  mapply(nouns, objects, SIMPLIFY = FALSE, FUN = function(noun, object) {
    c(noun, object)
  })
}


#' Recombine row and column labels
#'
#' This function recombines (unsplits) row or column labels that have
#' been separated by `split_labels()`.
#'
#' @param ls A vector of split row or column labels, probably created by `split_labels()`.
#' @param notation The notation object that describes the labels.
#'                 Default is `RCLabels::bracket_notation`.
#'
#' @return Recombined row and column labels.
#'
#' @export
#'
#' @examples
#' labs <- c("a [of b in c]", "d [from Coal mines in USA]")
#' labs
#' split <- split_labels(labs)
#' split
#' paste_pieces(split)
#' # Also works in a data frame
#' df <- tibble::tibble(labels = c("a [in b]", "c [of d into USA]",
#'                                 "e [of f in g]", "h [-> i in j]"))
#' recombined <- df |>
#'   dplyr::mutate(
#'     splits = split_labels(labels),
#'     recombined = paste_pieces(splits)
#'   )
#' all(recombined$labels == recombined$recombined)
paste_pieces <- function(ls, notation = RCLabels::bracket_notation) {
  nouns <- ls |>
    sapply(FUN = function(this_label) {
      this_label[["noun"]]
    })
  pps <- ls |>
    sapply(FUN = function(this_label) {
      without_noun <- this_label |>
        as.list() |>
        purrr::list_modify("noun" = NULL)
      paste0(names(without_noun), " ", without_noun, collapse = " ")
    })
  paste_pref_suff(pref = nouns, suff = pps, notation = notation)
}

