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
# Bracket arrow notation: a [-> b]
#

bracket_arrow_notation <- preposition_notation(preposition = "->")
usethis::use_data(bracket_arrow_notation, overwrite = TRUE)


#
# Prepositions
#

prepositions <- c("in", "into", "from", "of", "->")
usethis::use_data(prepositions, overwrite = TRUE)
