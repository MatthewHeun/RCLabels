#' Set nouns in labels
#'
#' This function sets the nouns of row and column labels.
#' The length of `new_nouns` must be the same as the length of `labels`.
#'
#' @param labels The row and column labels in which the nouns will be set.
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


#' Replace pieces of row and column labels
#'
#' This function replaces pieces of row and column labels
#' according to `label_map` that defines "one or many to one" relationships.
#' This function is useful for aggregations.
#' For example, aggregating by nouns can be done by
#' `label_map = list(new_noun = c("a", "b", "c"))`.
#' The string "new_noun" will replace any of "a", "b", or "c"
#' when they appear as nouns in a row or column label.
#' See examples for details.
#'
#' @param labels The row and column labels in which pieces will be replaced.
#' @param piece The piece of the row or column label that will be replaced.
#'              Typical examples are "noun" or a preposition,
#'              such as "in" or "from".
#'              See `RCLabels::prepositions` for additional examples.
#'              This argument may be a single string or a character vector.
#' @param label_map A named list of character vectors in which names indicate
#'                  strings to be inserted and values indicate
#'                  values that should be replaced.
#'
#' @return `labels` with replacements according to `label_map`.
#'
#' @export
#'
#' @examples
modify_label_pieces <- function(labels, piece, label_map) {
  splitted <- labels |>
    split_labels()
  # modified <- splitted |>
  #   lapply(FUN = function(this_split_label) {
  #     purrr::modify_at(this_split_label, .at = piece, .f = function(this_piece) {
  #       mapply(label_map, names(label_map), FUN = function(this_modification, this_modification_name) {
  #         if (this_piece %in% this_modification) {
  #           this_modification_name
  #         } else {
  #           this_piece
  #         }
  #       })
  #     })
  #
  #   })

  # Loop over everything to modify pieces.
  modified <- lapply(splitted, FUN = function(this_label) {
    # Loop over each piece
    for (i_piece in 1:length(this_label)) {
      this_piece <- this_label[i_piece]
      this_piece_name <- names(this_piece)
      if (this_piece_name %in% piece) {
        # Loop over all parts of the label_map
        for (i_replacement in 1:length(label_map)) {
          this_replacement <- label_map[[i_replacement]]
          this_replacement_name <- names(label_map[i_replacement])
          if (this_piece %in% this_replacement) {
            this_label[[this_piece_name]] <- this_replacement_name
          }
        }
      }
    }
    this_label
  })

  modified |>
    recombine_labels()
}
