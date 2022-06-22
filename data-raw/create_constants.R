## code to prepare `DATASET` dataset goes here


#
# Arrow notation: a -> b
#

arrow_notation <- notation_vec(sep = " -> ")
usethis::use_data(arrow_notation, overwrite = TRUE)


#
# Paren notation: a (b)
#

paren_notation <- notation_vec(sep = "",
                               pref_start = "",
                               pref_end = " (",
                               suff_start = " (",
                               suff_end = ")")
usethis::use_data(paren_notation, overwrite = TRUE)


#
# Bracket notation: a [b]
#

bracket_notation <- notation_vec(sep = "",
                                 pref_start = "",
                                 pref_end = " [",
                                 suff_start = " [",
                                 suff_end = "]")
usethis::use_data(bracket_notation, overwrite = TRUE)


#
# From notation: a [from b]
#

from_notation <- preposition_notation(preposition = "from")
usethis::use_data(from_notation, overwrite = TRUE)


#
# Of notation: a [of b]
#

of_notation <- preposition_notation(preposition = "of")
usethis::use_data(of_notation, overwrite = TRUE)


#
# To notation: a [to b]
#

to_notation <- preposition_notation(preposition = "to")
usethis::use_data(to_notation, overwrite = TRUE)


#
# In notation: a [in b]
#

in_notation <- preposition_notation(preposition = "in")
usethis::use_data(in_notation, overwrite = TRUE)


#
# Bracket arrow notation: a [-> b]
#

bracket_arrow_notation <- preposition_notation(preposition = "->")
usethis::use_data(bracket_arrow_notation, overwrite = TRUE)


#
# First dot notation: a.b
#

first_dot_notation <- notation_vec(sep = ".")
usethis::use_data(first_dot_notation, overwrite = TRUE)


#
# List of all known notations
#
# Note that the order of this list is important.
# For some labels (such as "a [from b]"),
# more than one notation could be inferred
# (such as bracket_notation and from_notation).
# Some code relies on the fact that the simplest notations are earlier,
# while more complex notations are later in this list.
# For example,
# infer_notation("a [from b]", choose_most_specific = FALSE) will return bracket_notation.
# infer_notation("a [from b]") with default choose_most_specific = TRUE will return from_notation.
#
# DO NOT CHANGE THE ORDER OF THIS LIST.

notations_list <- list(first_dot_notation = first_dot_notation,
                       arrow_notation = arrow_notation,
                       paren_notation = paren_notation,
                       bracket_notation = bracket_notation,
                       from_notation = from_notation,
                       of_notation = of_notation,
                       to_notation = to_notation,
                       in_notation = in_notation,
                       bracket_arrow_notation = bracket_arrow_notation)
usethis::use_data(notations_list, overwrite = TRUE)


#
# Prepositions
#

prepositions_list <- c("in", "into", "from", "of", "->", "to")
usethis::use_data(prepositions_list, overwrite = TRUE)



