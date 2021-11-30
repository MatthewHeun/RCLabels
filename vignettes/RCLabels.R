## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(RCLabels)

## -----------------------------------------------------------------------------
# Create a notation object.
my_notation <- notation_vec(pref_start = "(", pref_end = ") ",
                            suff_start = "[", suff_end = "]")

# Notation objects are character vectors.
my_notation

## -----------------------------------------------------------------------------
arrow_notation
paren_notation
bracket_notation
from_notation
of_notation
to_notation
bracket_arrow_notation

## -----------------------------------------------------------------------------
my_label <- paste_pref_suff(pref = "Coal", suff = "from Coal mines in USA", 
                            notation = my_notation)
my_label

## -----------------------------------------------------------------------------
# Split the prefix from the suffix to obtain a named list of strings.
split_pref_suff(my_label, notation = my_notation)

# Flip the prefix and suffix, maintaining the same notation.
flip_pref_suff(my_label, notation = my_notation)

# Change the notation.
switch_notation(my_label, from = my_notation, to = paren_notation)

# Change the notation and flip the prefix and suffix.
switch_notation(my_label, from = my_notation, to = paren_notation, flip = TRUE)

## -----------------------------------------------------------------------------
keep_pref_suff(my_label, keep = "pref", notation = my_notation)
keep_pref_suff(my_label, keep = "suff", notation = my_notation)

## -----------------------------------------------------------------------------
labels <- c("a [of b in c]", "d [of e in f]", "g [of h in i]")
labels

split_pref_suff(labels, notation = bracket_notation)

## -----------------------------------------------------------------------------
labels

df <- tibble::tibble(labels = labels)
result <- df |> 
  dplyr::mutate(
    split = split_pref_suff(labels, notation = bracket_notation, transpose = TRUE)
  )
result$split[[1]]
result$split[[2]]
result$split[[3]]

## -----------------------------------------------------------------------------
prepositions

## -----------------------------------------------------------------------------
labels

# Extract the nouns.
get_nouns(labels, notation = bracket_notation)

# Extract the prepositional phrases.
get_pps(labels, notation = bracket_notation)

# Extract the prepositions themselves.
get_prepositions(labels, notation = bracket_notation)

# Extract the objects of the prepositions.
# Objects are named by the preposition of their phrase.
get_objects(labels, notation = bracket_notation)

# Split the labels into pieces, named by "noun" and prepositions.
split_labels <- split_labels(labels, 
                             prepositions = prepositions, 
                             notation = bracket_notation)
split_labels

# Recombine split labels.
recombine_labels(split_labels, notation = bracket_notation)

# Recombine with a new notation.
recombine_labels(split_labels, notation = paren_notation)

## -----------------------------------------------------------------------------
labels

# Set new values for nouns.
modify_nouns(labels, 
             new_nouns = c("Coal", "Oil", "Natural gas"), 
             notation = bracket_notation)

## -----------------------------------------------------------------------------
labels

# Change nouns in several labels to "Production" and "Manufacture",
# as indicated by the modification map.
modify_label_pieces(labels, 
                    piece = "noun", 
                    mod_map = list(Production = c("a", "b", "c", "d"),
                                   Manufacture = c("g", "h", "i", "j")), 
                    notation = bracket_notation)

# Change the objects of the "in" preposition, 
# according to the modification map.
modify_label_pieces(labels, 
                    piece = "in", 
                    mod_map = list(GHA = "c", ZAF = c("f", "i")), 
                    notation = bracket_notation)

# Change the objects of "of" prepositions,
# according to the modification map.
modify_label_pieces(labels, 
                    piece = "of", 
                    mod_map = list(Coal = "b", `Crude oil` = c("e", "h")), 
                    notation = bracket_notation)

## -----------------------------------------------------------------------------
labels

# Eliminate all of the prepositional phrases that begin with "in".
remove_label_pieces(labels, 
                    piece = "in", 
                    notation = bracket_notation)

# Eliminate all of the prepositional phrases that begin with "of" and "in".
# Note that some spaces remain.
remove_label_pieces(labels, 
                    piece = c("of", "in"), 
                    notation = bracket_notation)

