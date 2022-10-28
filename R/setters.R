#' Modify nouns in labels
#'
#' This function modifies the nouns of row and column labels.
#' The length of `new_nouns` must be the same as the length of `labels`.
#'
#' @param labels The row and column labels in which the nouns will be modified.
#' @param new_nouns The new nouns to be set in `labels`.
#'                  Must be same length as `labels`.
#' @param inf_notation A boolean that tells whether to infer notation for `labels`.
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
#'
#' @return A character vector of same length as labels
#'         with nouns modified to be `new_nouns`.
#'
#' @export
#'
#' @examples
#' labels <- c("a [of b in c]", "d [of e in USA]")
#' modify_nouns(labels, c("a_plus", "g"))
modify_nouns <- function(labels,
                         new_nouns,
                         inf_notation = TRUE,
                         notation = RCLabels::notations_list,
                         choose_most_specific = FALSE) {
  num_labels <- length(labels)
  num_new_nouns <- length(new_nouns)
  if (num_labels != num_new_nouns) {
    stop("The number of labels must equal the number of new nouns in set_nouns()")
  }
  split <- split_noun_pp(labels, inf_notation = inf_notation, notation = notation, choose_most_specific = choose_most_specific)
  split_with_new_nouns <- mapply(split, new_nouns, SIMPLIFY = FALSE, FUN = function(this_split_label, this_new_noun) {
    this_split_label[["noun"]] <- this_new_noun
    this_split_label
  })
  # Build the new outgoing labels,
  # but do it based on whether or not the caller wanted to infer notations.
  inferred_notation <- infer_notation(labels, inf_notation = inf_notation, notations = notation, choose_most_specific = choose_most_specific)
  paste_noun_pp(split_with_new_nouns, inferred_notation)
}


#' Modify pieces of row and column labels
#'
#' Typical `piece`s include "noun" or a preposition,
#' such as "in" or "from".
#' See `RCLabels::prepositions` for additional examples.
#' This argument may be a single string or a character vector.
#'
#' This function modifies pieces of row and column labels
#' according to `label_map` that defines "one or many to one" relationships.
#' This function is useful for aggregations.
#' For example, replacing nouns can be done by
#' `modify_label_pieces(labels, piece = "noun", label_map = list(new_noun = c("a", "b", "c"))`.
#' The string "new_noun" will replace any of "a", "b", or "c"
#' when they appear as nouns in a row or column label.
#' See examples for details.
#'
#' The `mod_map` argument should consist of a
#' named list of character vectors in which names indicate
#' strings to be inserted and values indicate
#' values that should be replaced.
#' The sense is `new = old` or `new = olds`,
#' where "new" is the new name (the replacement) and
#' "old"/"olds" is/are a string/vector of strings,
#' any one of which will be replaced by "new".
#'
#' Note `piece` can be "pref"/"suff" or "noun"/"prepositions"
#' If any `piece` is "pref" or "suff",
#' all pieces are assumed to be a prefix or a suffix.
#' If non of the `piece`s are "pref" or "suff",
#' all `piece`s are assumed to be nouns or prepositions,
#' such as "in" or "from".
#' See `RCLabels::prepositions` for additional examples.
#' This argument may be a single string or a character vector.
#'
#' @param labels A vector of row or column labels in which pieces will be modified.
#' @param piece The piece (or pieces) of the row or column label that will be modified.
#' @param mod_map A modification map. See details.
#' @param prepositions A list of prepositions, used to detect prepositional phrases.
#'                     Default is `RCLabels::prepositions_list`.
#' @param inf_notation A boolean that tells whether to infer notation for `x`.
#'                     Default is `TRUE`.
#'                     See `infer_notation()` for details.
#' @param notation The notation type to be used when extracting prepositions.
#'                 Default is `RCLabels::notations_list`, meaning that
#'                 the notation is inferred using `infer_notation()`.
#' @param choose_most_specific A boolean that tells whether the most specific
#'                             notation is selected when more than one notation match.
#'                             Default is `FALSE`.
#'
#' @return `labels` with replacements according to `piece` and `mod_map`.
#'
#' @export
#'
#' @examples
#' # Simple case
#' modify_label_pieces("a [of b in c]",
#'                     piece = "noun",
#'                     mod_map = list(new_noun = c("a", "b")))
#' # Works with a vector or list of labels
#' modify_label_pieces(c("a [of b in c]", "d [-> e in f]"),
#'                     piece = "noun",
#'                     mod_map = list(new_noun = c("d", "e")))
#' # Works with multiple items in the mod_map
#' modify_label_pieces(c("a [of b in c]", "d [-> e in f]"),
#'                     piece = "noun",
#'                     mod_map = list(new_noun1 = c("a", "b", "c"),
#'                                    new_noun2 = c("d", "e", "f")))
#' # Works with multiple pieces to be modified
#' modify_label_pieces(c("a [of b in c]", "d [-> e in f]"),
#'                     piece = c("noun", "in"),
#'                     mod_map = list(new_noun = c("a", "b", "c"),
#'                                    new_in   = c("c", "f")))
modify_label_pieces <- function(labels,
                                piece,
                                mod_map,
                                prepositions = RCLabels::prepositions_list,
                                inf_notation = TRUE,
                                notation = RCLabels::bracket_notation,
                                choose_most_specific = FALSE) {

  notation <- infer_notation(labels,
                             inf_notation = inf_notation,
                             notations = notation,
                             allow_multiple = FALSE,
                             retain_names = FALSE,
                             choose_most_specific = choose_most_specific,
                             must_succeed = TRUE)

  # If the piece is "pref" or "suff", split along those lines.
  if (any(piece %in% c("pref", "suff"))) {
    split <- labels %>%
      # Already inferred notation
      split_pref_suff(inf_notation = FALSE, notation = notation, choose_most_specific = choose_most_specific, transpose = TRUE)
  } else {
    # The caller didn't request "pref" or "suff", so try to split by noun and prepositional phrase.
    split <- labels %>%
      # Already inferred notation
      split_noun_pp(inf_notation = FALSE, notation = notation, prepositions = prepositions, choose_most_specific = choose_most_specific)
  }

  # Loop over everything to modify pieces.
  modified <- lapply(split, FUN = function(this_label) {
    # Loop over each piece in this_label
    for (i_piece in 1:length(this_label)) {
      this_piece <- this_label[[i_piece]]
      this_piece_name <- names(this_label[i_piece])
      if (this_piece_name %in% piece) {
        # Loop over all parts of the mod_map
        for (i_replacement in 1:length(mod_map)) {
          this_replacement <- mod_map[[i_replacement]]
          this_replacement_name <- names(mod_map[i_replacement])
          if (this_piece %in% this_replacement) {
            this_label[[this_piece_name]] <- this_replacement_name
          }
        }
      }
    }
    this_label
  })

  if (any(piece %in% c("pref", "suff"))) {
    out <- lapply(modified, paste_pref_suff)
    if (!is.list(labels)) {
      out <- unlist(out)
    }
  } else {
    out <- modified %>%
      paste_noun_pp(notation = notation)
  }
  return(out)
}


#' Remove a prepositional phrase in a row or column label
#'
#' This function removes pieces from
#' row and column labels.
#'
#' @param labels The row and column labels from which prepositional phrases will be removed.
#' @param pieces_to_remove The names of pieces of the label to be removed,
#'                         typically "noun" or a preposition such as "of" or "in"
#'                         See `RCLabels::prepositions_list` for a list of known prepositions.
#' @param prepositions A list of prepositions, used to detect prepositional phrases.
#'                     Default is `RCLabels::prepositions_list`.
#' @param inf_notation A boolean that tells whether to infer notation for `x`.
#'                     Default is `TRUE`.
#'                     See `infer_notation()` for details.
#' @param notation The notation type to be used when extracting prepositions.
#'                 Default is `RCLabels::notations_list`, meaning that
#'                 the notation is inferred using `infer_notation()`.
#' @param choose_most_specific A boolean that tells whether the most specific
#'                             notation is selected when more than one notation match.
#'                             Default is `FALSE`.
#'
#' @return `labels` with pieces removed.
#'
#' @export
#'
#' @examples
#' labs <- c("a [of b in c]", "d [-> e in f]")
#' remove_label_pieces(labs, pieces_to_remove = "of")
#' remove_label_pieces(labs, pieces_to_remove = c("of", "->"))
#' remove_label_pieces(labs, pieces_to_remove = c("in", "into"))
#' remove_label_pieces(labs, pieces_to_remove = c("of", "in"))
remove_label_pieces <- function(labels,
                                pieces_to_remove,
                                prepositions = RCLabels::prepositions_list,
                                inf_notation = TRUE,
                                notation = RCLabels::notations_list,
                                choose_most_specific = FALSE) {
  notation <- infer_notation(labels,
                             inf_notation = inf_notation,
                             notations = notation,
                             allow_multiple = FALSE,
                             retain_names = FALSE,
                             choose_most_specific = FALSE,
                             must_succeed = TRUE)
  split <- labels %>%
    # Already inferred the notation
    split_noun_pp(inf_notation = FALSE, notation = notation, prepositions = prepositions)

  for (i_split_label in 1:length(split)) {
    this_split_label <- split[[i_split_label]]
    # Go backwards so as not to mess up indexing when removing pieces.
    for (i_piece in length(this_split_label):1) {
      this_split_label_name <- names(this_split_label[i_piece])
      if (this_split_label_name %in% pieces_to_remove) {
        this_split_label <- this_split_label[-i_piece]
      }
    }
    split[[i_split_label]] <- this_split_label
  }
  split %>%
    paste_noun_pp(notation = notation)
}
