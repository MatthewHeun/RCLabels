#' Arrow notation
#'
#' A description of arrow notation.
#'
#' @format A vector of notational symbols that provides an arrow separator ("a -> b")
#'         between prefix and suffix.
#' @examples
#' arrow_notation
"arrow_notation"


#' A description of dash notation.
#'
#' @format A vector of notational symbols that provides an dash separator ("a - b")
#'         between prefix and suffix.
#' @examples
#' dash_notation
"dash_notation"


#' Parenthetical notation
#'
#' A description of parenthetical notation.
#'
#' @format A vector of notational symbols that provides a parenthetical ("a (b)") notation.
#' @examples
#' paren_notation
"paren_notation"


#' Bracket notation
#'
#' A description of bracket notation.
#'
#' @format A vector of notational symbols that provides bracket ("a \[b\]") notation.
#' @examples
#' bracket_notation
"bracket_notation"


#' From notation
#'
#' A description of from notation.
#'
#' @format A vector of notational symbols that provides from ("a \[from b\]") notation.
#' @examples
#' from_notation
"from_notation"


#' Of notation
#'
#' A description of of notation.
#'
#' @format A vector of notational symbols that provides of ("a \[of b\]") notation.
#' @examples
#' of_notation
"of_notation"


#' To notation
#'
#' A description of to notation.
#'
#' @format A vector of notational symbols that provides to ("a \[to b\]") notation.
#' @examples
#' to_notation
"to_notation"


#' In notation
#'
#' A description of in notation.
#'
#' @format A vector of notational symbols that provides to ("a \[in b\]") notation.
#' @examples
#' in_notation
"in_notation"


#' Bracket arrow notation
#'
#' A description of bracket arrow notation.
#'
#' @format A vector of notational symbols that provides bracket arrow ("a \[-> b\]") notation.
#' @examples
#' bracket_arrow_notation
"bracket_arrow_notation"



#' First dot notation
#'
#' A description of first dot notation.
#' Note that "a.b.c" splits into prefix ("a") and suffix ("b.c").
#'
#' @format A vector of notational symbols that provides first dot ("a.b") notation.
#' @examples
#' first_dot_notation
"first_dot_notation"


#' Notations list
#'
#' A list of all bundled notations.
#' This list is organized by least specific to most specific,
#' thereby enabling some unique behaviors in `infer_notation()`.
#' See the examples for `infer_notation()`.
#'
#' @format A list of bundled notations.
#' @examples
#' notations_list
"notations_list"


#' Prepositions
#'
#' Prepositions used in row and column labels.
#'
#' @format A vector of prepositions used in row and column labels.
#' @examples
#' prepositions_list
"prepositions_list"


#' Prepositions
#'
#' This constant is deprecated.
#' Please use `prepositiions_list` instead.
#'
#' @format A vector of prepositions used in row and column labels.
"prepositions"
