#' Extract nouns from row and column labels
#'
#' Nouns are the first part of a row-column label,
#' "a" in "a \[b\]".
#' Internally, this function calls `get_pref_suff(which = "pref")`.
#'
#' @param labels A list or vector of labels from which nouns are to be extracted.
#' @param inf_notation A boolean that tells whether to infer notation for `x`.
#'                     Default is `TRUE`.
#'                     See `infer_notation()` for details.
#' @param notation The notation type to be used when extracting nouns.
#'                 Default is `RCLabels::notations_list`, meaning that
#'                 the notation is inferred using `infer_notation()`.
#' @param choose_most_specific A boolean that tells whether to choose the most specific
#'                             notation from `notation` when inferring notation.
#'                             Default is `TRUE`.
#'
#' @return A list of nouns from row and column labels.
#'
#' @export
#'
#' @examples
#' get_nouns("a [b]", notation = bracket_notation)
#' # Also works with vectors and lists.
#' get_nouns(c("a [b]", "c [d]"))
#' get_nouns(list("a [b]", "c [d]"))
get_nouns <- function(labels,
                      inf_notation = TRUE,
                      notation = RCLabels::notations_list,
                      choose_most_specific = TRUE) {
  if (is.null(labels)) {
    return(NULL)
  }
  get_pref_suff(labels,
                which = "pref",
                inf_notation = inf_notation,
                notation = notation,
                choose_most_specific = choose_most_specific) %>%
    magrittr::set_names(rep("noun", length(labels)))
}


#' Extract prepositional phrases of row and column labels
#'
#' This function extracts prepositional phrases from suffixes of row and column labels
#' of the form "a \[preposition b\]", where "preposition b" is the prepositional phrase.
#'
#' @param labels A list or vector of labels from which prepositional phrases are to be extracted.
#' @param inf_notation A boolean that tells whether to infer notation for `x`.
#'                     Default is `TRUE`.
#'                     See `infer_notation()` for details.
#' @param notation The notation type to be used when extracting prepositional phrases.
#'                 Default is `RCLabels::notations_list`, meaning that
#'                 the notation is inferred using `infer_notation()`.
#' @param choose_most_specific A boolean that tells whether to choose the most specific
#'                             notation from `notation` when inferring notation.
#'                             Default is `FALSE` so that a less specific notation can be
#'                             inferred.
#'                             In combination with `RCLabels::notations_list`,
#'                             the default value of `FALSE` means that
#'                             `RCLabels::bracket_notation` will be selected instead of
#'                             anything more specific, such as
#'                             `RCLabels::from_notation`.
#' @param prepositions A list of prepositions for which to search.
#'                     Default is `RCLabels::prepositions_list`.
#'
#' @return All prepositional phrases in a suffix.
#'
#' @export
#'
#' @examples
#' get_pps(c("a [in b]", "c [of d]"))
#' get_pps(c("a [of b in c]", "d [-> e of f]"))
get_pps <- function(labels,
                    inf_notation = TRUE,
                    notation = RCLabels::notations_list,
                    choose_most_specific = FALSE,
                    prepositions = RCLabels::prepositions_list) {
  if (is.null(labels)) {
    return(NULL)
  }
  suffixes <- get_pref_suff(labels, which = "suff",
                            inf_notation = inf_notation,
                            notation = notation,
                            choose_most_specific = choose_most_specific)
  # Location prepositions
  preposition_words <- paste0(prepositions, " ")
  prep_patterns <- make_or_pattern(preposition_words,
                                   pattern_type = "anywhere")
  start_locations <- gregexpr(prep_patterns, text = suffixes) %>%
    unlist()
  if (length(start_locations) == 1) {
    end_locations <- nchar(suffixes)
  } else {
    end_locations <- c(start_locations[-1] - 2, nchar(suffixes))
  }
  substring(suffixes,
            first = start_locations[[1]],
            last = end_locations[[length(end_locations)]]) %>%
    magrittr::set_names(rep("pps", length(labels)))
}


#' Extract prepositions from row and column labels
#'
#' This function extracts prepositions from a list of row and column labels.
#' The list has outer structure of the number of labels and
#' an inner structure of each prepositional phrase in the specific label.
#'
#' If labels are in the form of
#' [RCLabels::from_notation], [RCLabels::to_notation] or similar,
#' it is probably best to give [RCLabels::bracket_notation] in the `notation`
#' argument.
#' Providing
#' [RCLabels::from_notation], [RCLabels::to_notation] or similar
#' in the `notation` argument will lead to empty results.
#' The preposition is discarded when extracting the suffix,
#' yielding empty strings for the prepositions.
#'
#' @param labels The row and column labels from which prepositional phrases are to be extracted.
#' @param inf_notation A boolean that tells whether to infer notation for `x`.
#'                     Default is `TRUE`.
#'                     See `infer_notation()` for details.
#' @param notation The notation type to be used when extracting prepositions.
#'                 Default is `RCLabels::notations_list`, meaning that
#'                 the notation is inferred using `infer_notation()`.
#' @param choose_most_specific A boolean that tells whether to choose the most specific
#'                             notation from `notation` when inferring notation.
#'                             Default is `FALSE` so that a less specific notation can be
#'                             inferred.
#'                             In combination with `RCLabels::notations_list`,
#'                             the default value of `FALSE` means that
#'                             `RCLabels::bracket_notation` will be selected instead of
#'                             anything more specific, such as
#'                             `RCLabels::from_notation`.
#' @param prepositions A vector of strings to be treated as prepositions.
#'                     Note that a space is appended to each word internally,
#'                     so, e.g., "to" becomes "to ".
#'                     Default is `RCLabels::prepositions_list`.
#'
#' @return A list of prepositions.
#'
#' @export
#'
#' @examples
#' get_prepositions(c("a [of b into c]", "d [-> e of f]"))
#' get_prepositions(c("a [of b]", "d [-> e of f]"),
#'                  inf_notation = FALSE,
#'                  notation = bracket_notation)
#' # Best to *not* specify notation by the preposition,
#' # as the result will be empty strings.
#' # Rather, give the notation as `bracket_notation`
#' # as shown above, or infer the notation
#' # as shown below.
#' get_prepositions(c("a [of b]", "d [-> e of f]"),
#'                  inf_notation = TRUE)
#' # The suffix is extracted, and the preposition
#' # is lost before looking for the preposition.
#' get_prepositions(c("a [of b]", "d [of f]"),
#'                  inf_notation = FALSE,
#'                  notation = of_notation)
get_prepositions <- function(labels,
                             inf_notation = TRUE,
                             notation = RCLabels::notations_list,
                             choose_most_specific = FALSE,
                             prepositions = RCLabels::prepositions_list) {
  if (is.null(labels)) {
    return(NULL)
  }
  pps <- get_pref_suff(labels,
                       which = "suff",
                       inf_notation = inf_notation,
                       notation = notation,
                       choose_most_specific = choose_most_specific)
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
  out <- out %>%
    magrittr::set_names(rep("prepositions", length(labels)))
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
#' @param inf_notation A boolean that tells whether to infer notation for `x`.
#'                     Default is `TRUE`.
#'                     See `infer_notation()` for details.
#' @param notation The notation type to be used when extracting prepositions.
#'                 Default is `RCLabels::notations_list`, meaning that
#'                 the notation is inferred using `infer_notation()`.
#' @param choose_most_specific A boolean that tells whether to choose the most specific
#'                             notation from `notation` when inferring notation.
#'                             Default is `FALSE` so that a less specific notation can be
#'                             inferred.
#'                             In combination with `RCLabels::notations_list`,
#'                             the default value of `FALSE` means that
#'                             `RCLabels::bracket_notation` will be selected instead of
#'                             anything more specific, such as
#'                             `RCLabels::from_notation`.
#' @param prepositions A vector of strings to be treated as prepositions.
#'                     Note that a space is appended to each word internally,
#'                     so, e.g., "to" becomes "to ".
#'                     Default is `RCLabels::prepositions_list`.
#'
#' @return A list of objects of prepositional phrases,
#'         with names being prepositions, and values being objects.
#'
#' @export
#'
#' @examples
#' get_objects(c("a [of b into c]", "d [of Coal from e -> f]"))
get_objects <- function(labels,
                        inf_notation = TRUE,
                        notation = RCLabels::notations_list,
                        choose_most_specific = FALSE,
                        prepositions = RCLabels::prepositions_list) {
  if (is.null(labels)) {
    return(NULL)
  }
  pps <- get_pref_suff(labels,
                       which = "suff",
                       inf_notation = inf_notation,
                       notation = notation,
                       choose_most_specific = choose_most_specific)
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

  these_prepositions <- get_prepositions(labels,
                                         inf_notation = inf_notation,
                                         notation = notation,
                                         prepositions = prepositions)

  mapply(pps, obj_start_locations, obj_end_locations, these_prepositions,
                    SIMPLIFY = FALSE, USE.NAMES = FALSE, FUN = function(this_pp, these_osls, these_oels, these_pps) {
    mapply(these_osls, these_oels, these_pps, FUN = function(osl, oel, these_preps) {
      substring(this_pp, first = osl, last = oel) %>%
        magrittr::set_names(these_preps)
    })
  }) %>%
    magrittr::set_names(rep("objects", length(labels)))
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
#' argument on `split_noun_pp()`.
#' Labels may not have the same structure,
#' e.g., they may have different prepositions.
#'
#' @param labels The row and column labels from which prepositional phrases are to be extracted.
#' @param inf_notation A boolean that tells whether to infer notation for `x`.
#'                     Default is `TRUE`.
#'                     See `infer_notation()` for details.
#' @param notation The notation type to be used when extracting prepositions.
#'                 Default is `RCLabels::notations_list`, meaning that
#'                 the notation is inferred using `infer_notation()`.
#' @param choose_most_specific A boolean that tells whether to choose the most specific
#'                             notation from `notation` when inferring notation.
#'                             Default is `FALSE` so that a less specific notation can be
#'                             inferred.
#'                             In combination with `RCLabels::notations_list`,
#'                             the default value of `FALSE` means that
#'                             `RCLabels::bracket_notation` will be selected instead of
#'                             anything more specific, such as
#'                             `RCLabels::from_notation`.
#' @param prepositions A vector of strings to be treated as prepositions.
#'                     Note that a space is appended to each word internally,
#'                     so, e.g., "to" becomes "to ".
#'                     Default is `RCLabels::prepositions_list`.
#'
#' @return A list of lists with items named `noun` and `pp`.
#'
#' @export
#'
#' @examples
#' # Specify the notation
#' split_noun_pp(c("a [of b in c]", "d [of e into f]"),
#'               notation = bracket_notation)
#' # Infer the notation via default arguments
#' split_noun_pp(c("a [of b in c]", "d [of e into f]"))
split_noun_pp <- function(labels,
                          inf_notation = TRUE,
                          notation = RCLabels::notations_list,
                          choose_most_specific = FALSE,
                          prepositions = RCLabels::prepositions_list) {
  if (is.null(labels)) {
    return(NULL)
  }
  nouns <- get_nouns(labels,
                     inf_notation = inf_notation,
                     notation = notation,
                     choose_most_specific = choose_most_specific) %>%
    as.list() %>%
    unname() %>%
    lapply(FUN = function(this_noun) {
      magrittr::set_names(this_noun, "noun")
    })
  objects <- get_objects(labels,
                         inf_notation = inf_notation,
                         notation = notation,
                         prepositions = prepositions,
                         choose_most_specific = choose_most_specific)

  mapply(nouns, objects, SIMPLIFY = FALSE, FUN = function(noun, object) {
    c(noun, object)
  })
}


#' Recombine row and column labels
#'
#' This function recombines (unsplits) row or column labels that have
#' been separated by `split_noun_pp()`.
#'
#' @param splt_labels A vector of split row or column labels, probably created by `split_noun_pp()`.
#' @param notation The notation object that describes the labels.
#'                 Default is `RCLabels::bracket_notation`.
#' @param squish A boolean that tells whether to remove extra spaces in the output of `paste_*()` functions.
#'               Default is `TRUE`.
#'
#' @return Recombined row and column labels.
#'
#' @export
#'
#' @examples
#' labs <- c("a [of b in c]", "d [from Coal mines in USA]")
#' labs
#' split <- split_noun_pp(labs)
#' split
#' paste_noun_pp(split)
#' # Also works in a data frame
#' df <- tibble::tibble(labels = c("a [in b]", "c [of d into USA]",
#'                                 "e [of f in g]", "h [-> i in j]"))
#' recombined <- df %>%
#'   dplyr::mutate(
#'     splits = split_noun_pp(labels),
#'     recombined = paste_noun_pp(splits)
#'   )
#' all(recombined$labels == recombined$recombined)
paste_noun_pp <- function(splt_labels, notation = RCLabels::bracket_notation, squish = TRUE) {
  if (is.null(splt_labels)) {
    return(NULL)
  }
  nouns <- splt_labels %>%
    sapply(FUN = function(this_label) {
      this_label[["noun"]]
    })
  pps <- sapply(splt_labels, FUN = function(this_label) {
    without_noun <- this_label[setdiff(names(this_label), "noun")]
    paste0(names(without_noun), " ", without_noun, collapse = " ")
  })
  paste_pref_suff(pref = nouns, suff = pps, notation = notation, squish = squish)
}


#' Get a piece of a label
#'
#' This is a wrapper function for `get_pref_suff()`, `get_nouns()`, and
#' `get_objects()`.
#' It returns a `piece` of a row or column label.
#'
#' `piece` is typically one of
#' * "all" (which returns `labels` directly),
#' * "pref" (for the prefixes),
#' * "suff" (for the suffixes),
#' * "noun" (returns the noun),
#' * "pps" (prepositional phrases, returns prepositional phrases in full),
#' * "prepositions" (returns a list of prepositions),
#' * "objects" (returns a list of objects with prepositions as names), or
#' * a preposition in `prepositions` (as a string), which will return
#'   the object of that preposition named by the preposition itself.
#'
#' `piece` must be a character vector of length 1.
#' If a `piece` is missing in a label, "" (empty string) is returned.
#'
#' If specifying more than one `notation`, be sure the notations are in a list.
#' `notation = c(RCLabels::bracket_notation, RCLabels::arrow_notation)`
#' is unlikely to produce the desired result, because the notations
#' are concatenated together to form a long string vector.
#' Rather say
#' `notation = list(RCLabels::bracket_notation, RCLabels::arrow_notation)`.
#'
#' @param labels The row and column labels from which prepositional phrases are to be extracted.
#' @param piece The name of the item to return.
#' @param inf_notation A boolean that tells whether to infer notation for `x`.
#'                     Default is `TRUE`.
#'                     See `infer_notation()` for details.
#' @param notation The notation type to be used when extracting prepositions.
#'                 Default is `RCLabels::notations_list`, meaning that
#'                 the notation is inferred using `infer_notation()`.
#' @param choose_most_specific A boolean that tells whether to choose the most specific
#'                             notation from `notation` when inferring notation.
#'                             Default is `FALSE` so that a less specific notation can be
#'                             inferred.
#'                             In combination with `RCLabels::notations_list`,
#'                             the default value of `FALSE` means that
#'                             `RCLabels::bracket_notation` will be selected instead of
#'                             anything more specific, such as
#'                             `RCLabels::from_notation`.
#' @param prepositions A vector of strings to be treated as prepositions.
#'                     Note that a space is appended to each word internally,
#'                     so, e.g., "to" becomes "to ".
#'                     Default is `RCLabels::prepositions_list`.
#'
#' @return A `piece` of `labels`.
#'
#' @export
#'
#' @examples
#' labs <- c("a [from b in c]", "d [of e in f]", "Export [of Coal from USA to MEX]")
#' get_piece(labs, "pref")
#' get_piece(labs, "suff")
#' get_piece(labs, piece = "noun")
#' get_piece(labs, piece = "pps")
#' get_piece(labs, piece = "prepositions")
#' get_piece(labs, piece = "objects")
#' get_piece(labs, piece = "from")
#' get_piece(labs, piece = "in")
#' get_piece(labs, piece = "of")
#' get_piece(labs, piece = "to")
get_piece <- function(labels,
                      piece = "all",
                      inf_notation = TRUE,
                      notation = RCLabels::notations_list,
                      choose_most_specific = FALSE,
                      prepositions = RCLabels::prepositions_list) {
  if (is.null(labels)) {
    return(NULL)
  }
  assertthat::assert_that(length(piece) == 1, msg = "piece must be a character vector of length 1 in RCLabels::get()")
  if (piece == "all") {
    return(labels)
  } else if (piece == "pref" | piece == "suff") {
    return(get_pref_suff(labels, which = piece, inf_notation = inf_notation, notation = notation, choose_most_specific = choose_most_specific))
  } else if (piece == "noun") {
    return(get_nouns(labels, notation = notation, inf_notation = inf_notation, choose_most_specific = choose_most_specific))
  } else if (piece == "pps") {
    return(get_pps(labels, notation = notation, inf_notation = inf_notation, choose_most_specific = choose_most_specific))
  } else if (piece == "prepositions") {
    return(get_prepositions(labels, notation = notation, inf_notation = inf_notation, choose_most_specific = choose_most_specific, prepositions = prepositions))
  } else if (piece == "objects") {
    return(get_objects(labels, notation = notation, inf_notation = inf_notation, choose_most_specific = choose_most_specific, prepositions = prepositions))
  }
  # If we get here, assume we want the object of a particular preposition
  out <- get_objects(labels, notation = notation, inf_notation = inf_notation, choose_most_specific = choose_most_specific, prepositions = prepositions)
  out <- lapply(out, FUN = function(pieces){
    theoneswewant <- pieces[names(pieces) == piece]
    if (length(theoneswewant) == 0) {
      return("" %>% magrittr::set_names(piece))
    }
    return(theoneswewant)
  })
  out %>%
    magrittr::set_names(rep(NULL, length(labels)))
}
