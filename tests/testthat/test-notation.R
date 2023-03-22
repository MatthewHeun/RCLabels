

test_that("arrow notation is correct", {
  an <- arrow_notation
  expect_equal(an[["pref_start"]], "")
  expect_equal(an[["pref_end"]], " -> ")
  expect_equal(an[["suff_start"]], " -> ")
  expect_equal(an[["suff_end"]], "")
})


test_that("paren notation is correct", {
  pn <- paren_notation
  expect_equal(pn[["pref_start"]], "")
  expect_equal(pn[["pref_end"]], " (")
  expect_equal(pn[["suff_start"]], " (")
  expect_equal(pn[["suff_end"]], ")")
})


test_that("bracket notation is correct", {
  bn <- bracket_notation
  expect_equal(bn[["pref_start"]], "")
  expect_equal(bn[["pref_end"]], " [")
  expect_equal(bn[["suff_start"]], " [")
  expect_equal(bn[["suff_end"]], "]")
})


test_that("preposition_notation() works as expected", {
  pn <- preposition_notation("foobar")
  expect_equal(pn[["pref_start"]], "")
  expect_equal(pn[["pref_end"]], " [foobar ")
  expect_equal(pn[["suff_start"]], " [foobar ")
  expect_equal(pn[["suff_end"]], "]")

  pn_parens <- preposition_notation("foobar", suff_start = "(", suff_end = "****")
  expect_equal(pn_parens[["pref_start"]], "")
  expect_equal(pn_parens[["pref_end"]], "(foobar ")
  expect_equal(pn_parens[["suff_start"]], "(foobar ")
  expect_equal(pn_parens[["suff_end"]], "****")
})


test_that("from_notation is correct", {
  fn <- from_notation
  expect_equal(fn[["pref_start"]], "")
  expect_equal(fn[["pref_end"]], " [from ")
  expect_equal(fn[["suff_start"]], " [from ")
  expect_equal(fn[["suff_end"]], "]")
})


test_that("of_notation is correct", {
  on <- of_notation
  expect_equal(on[["pref_start"]], "")
  expect_equal(on[["pref_end"]], " [of ")
  expect_equal(on[["suff_start"]], " [of ")
  expect_equal(on[["suff_end"]], "]")
})


test_that("in_notation is correct", {
  in_not <- in_notation
  expect_equal(in_not[["pref_start"]], "")
  expect_equal(in_not[["pref_end"]], " [in ")
  expect_equal(in_not[["suff_start"]], " [in ")
  expect_equal(in_not[["suff_end"]], "]")
})


test_that("first_dot notation is correct", {
  fd <- first_dot_notation
  expect_equal(fd[["pref_start"]], "")
  expect_equal(fd[["pref_end"]], ".")
  expect_equal(fd[["suff_start"]], ".")
  expect_equal(fd[["suff_end"]], "")
})


test_that("split_pref_suff() works properly", {
  expect_equal(RCLabels:::split_pref_suff("a -> b", notation = arrow_notation), list(pref = "a", suff = "b"))
  expect_equal(RCLabels:::split_pref_suff("b [a]", notation = bracket_notation), list(pref = "b", suff = "a"))

  # See if it works with a vector of strings
  expect_equal(RCLabels:::split_pref_suff(c("a -> b", "c -> d"), notation = arrow_notation),
               list(pref = c("a", "c"), suff = c("b", "d")))
  # See if it works with a list of strings
  expect_equal(RCLabels:::split_pref_suff(list("a -> b", "a -> b"), notation = arrow_notation),
               list(pref = c("a", "a"), suff = c("b", "b")))

  # See if it works when we don't have a suffix
  expect_equal(RCLabels:::split_pref_suff(list("a", "b"), notation = arrow_notation),
               list(pref = c("a", "b"), suff = c("", "")))

  # See if it works when we don't have a prefix or a suffix.
  expect_equal(RCLabels:::split_pref_suff(list(" -> ", " -> "), notation = arrow_notation),
               list(pref = c("", ""), suff = c("", "")))

  # See if it works when we don't have a delimiter.
  expect_equal(RCLabels:::split_pref_suff(list("a -> b", "r2", "r3"), notation = arrow_notation),
               list(pref = c("a", "r2", "r3"), suff = c("b", "", "")))

  # Try with unusual prefixes and suffixes
  nl <- notation_vec(pref_start = " {", pref_end = "} ", suff_start = "} ", suff_end = "")
  expect_equal(RCLabels:::split_pref_suff(" {a} bcd", notation = nl),
               list(pref = "a", suff = "bcd"))

  nl2 <- notation_vec(pref_start = "  {", pref_end = "} ", suff_start = "[ ", suff_end = "]  ")
  expect_equal(RCLabels:::split_pref_suff("  {a} [ b]  ", notation = nl2),
               list(pref = "a", suff = "b"))

  expect_equal(RCLabels:::split_pref_suff("a [ [b]]", notation = bracket_notation),
               list(pref = "a", suff = "b]"))

  # Try with degenerate cases
  nl3 <- notation_vec(sep = "{{}}")
  expect_equal(RCLabels:::split_pref_suff("abc {{} def", notation = nl3),
               list(pref = "abc {{} def", suff = ""))
  expect_equal(RCLabels:::split_pref_suff("abc {{}} def", notation = nl3),
               list(pref = "abc ", suff = " def"))

  # Try with weird parentheses
  nl4 <- notation_vec(pref_start = "(", pref_end = ")", suff_start = "(", suff_end = ")")
  expect_equal(RCLabels:::split_pref_suff("(a)(b)", notation = nl4),
               list(pref = "a", suff = "b"))

  expect_equal(RCLabels:::split_pref_suff("a b", notation = nl4),
               list(pref = "a b", suff = ""))
})


test_that("split_pref_suff() works with all structures of input", {
  expect_equal(split_pref_suff("a [b]", notation = bracket_notation), list(pref = "a", suff = "b"))

  expect_equal(split_pref_suff(c("a [b]", "c [d]"), notation = bracket_notation),
               list(pref = c("a", "c"), suff = c("b", "d")))

  expect_equal(split_pref_suff(list("a [b]", "c [d]"), notation = bracket_notation),
               list(pref = c("a", "c"), suff = c("b", "d")))

  expect_equal(split_pref_suff(c("a [b]", "c [d]", "e [f]", "g [h]"), notation = bracket_notation),
               list(pref = c("a", "c", "e", "g"),
                    suff = c("b", "d", "f", "h")))
})


test_that("split_pref_suff() works with full notation", {
  note <- notation_vec(pref_start = "(", pref_end = ")" , suff_start = "(", suff_end = ")")
  expect_equal(RCLabels:::split_pref_suff(c("(a)(b)", "(c)(d)"), notation = note),
               list(pref = c("a", "c"),
                    suff = c("b", "d")))
})


test_that("split_pref_suff() works first_dot notation", {
  expect_equal(split_pref_suff("A.B", notation = first_dot_notation),
               list(pref = "A",
                    suff = "B"))
  expect_equal(split_pref_suff("A.B.C", notation = first_dot_notation),
               list(pref = "A",
                    suff = "B.C"))
  expect_equal(split_pref_suff(c("a.b", "c.d.e", "f.g"), notation = first_dot_notation),
               list(pref = c("a", "c", "f"),
                    suff = c("b", "d.e", "g")))
})


test_that("split_pref_suff() works in a data frame", {
  df <- data.frame(donottouch = c(1, 2, 3), orig = c("a -> b", "c -> d", "e -> f"))
  split <- df %>%
    dplyr::mutate(
      split = RCLabels:::split_pref_suff(orig, notation = arrow_notation, transpose = TRUE)
    )
  expect_equal(split$split, list(list(pref = "a", suff = "b"),
                                 list(pref = "c", suff = "d"),
                                 list(pref = "e", suff = "f")))
})


test_that("split_pref_suff() works while inferring notation", {
  expect_equal(split_pref_suff("a [b]",
                               notation = RCLabels::notations_list),
               list(pref = "a", suff = "b"))
  expect_equal(split_pref_suff(c("a [from b]", "c [from d]"),
                               notation = RCLabels::from_notation),
               list(pref = c("a", "c"), suff = c("b", "d")))
  expect_equal(split_pref_suff(c("a [from b]", "c [from d]"),
                               notation = RCLabels::notations_list),
               list(pref = c("a", "c"), suff = c("b", "d")))
  expect_equal(split_pref_suff(c("a [b]", "c [from d]"),
                               notation = RCLabels::notations_list),
               list(pref = c("a", "c"), suff = c("b", "d")))
})


test_that("split_pref_suff() works with one successful and one unsuccessful inference", {
  expect_equal(split_pref_suff("abcde"), list(pref = "abcde", suff = ""))
  expect_equal(split_pref_suff(list("a [b]", "abcde")), list(pref = c("a", "abcde"), suff = c("b", "")))
  expect_equal(split_pref_suff(list("a [b]", "abcde"), notation = RCLabels::bracket_notation),
               list(pref = c("a", "abcde"), suff = c("b", "")))
  # For this one, a notation inference is requested.
  # But it will fail, because there is no known notation for "abcde".
  expect_equal(split_pref_suff(list("a [b]", "abcde")),
               list(pref = c("a", "abcde"), suff = c("b", "")))
})


test_that("split_pref_suff() works with 2 notations", {
  res <- split_pref_suff(c("a [b]", "c -> d", "e -> f"),
                         inf_notation = TRUE,
                         notation = list(RCLabels::bracket_notation, RCLabels::arrow_notation))
  expect_equal(res, list(pref = c("a", "c", "e"), suff = c("b", "d", "f")))
})


test_that("pathological case produces expected result for strip_label_part()", {
  expect_equal(RCLabels:::strip_label_part(c("a -> b", "c -> d"), part = "pref", notation = NULL),
               c("a -> b", "c -> d"))
  expect_equal(RCLabels:::strip_label_part(vector(mode = "character", length = 0), part = "pref", notation = NULL),
               character())
})


test_that("paste_pref_suff() works properly", {
  ps <- c(pref = "a", suff = "b")
  expect_equal(paste_pref_suff(ps, notation = arrow_notation), "a -> b")
  # Make sure that they are the inverse of each other,
  # except for converting to a list.
  expect_equal(paste_pref_suff(ps, notation = arrow_notation) %>%
                 split_pref_suff(notation = arrow_notation), as.list(ps))
  # Try with paren notation list
  expect_equal(paste_pref_suff(ps, notation = bracket_notation) %>%
                 split_pref_suff(notation = bracket_notation), as.list(ps))
  # Try with a wacky notation list
  amp_nl <- notation_vec(sep = "&&&&&&&&")
  expect_equal(paste_pref_suff(ps, notation = amp_nl) %>%
                 split_pref_suff(notation = amp_nl), as.list(ps))
  paren_nl <- notation_vec(pref_start = "(", pref_end = ")",
                           suff_start = "(", suff_end = ")")
  expect_equal(paste_pref_suff(ps, notation = paren_nl) %>%
                 split_pref_suff(notation = paren_nl), as.list(ps))
  # Try to paste lists of prefixes and suffixes
  expect_equal(paste_pref_suff(ps = list(list(pref = "a", suff = "b"), list(pref = "c", suff = "d"))),
               c("a -> b", "c -> d"))

  # Try to split then paste vectors
  pasted <- c("a -> b", "c -> d")
  expect_equal(split_pref_suff(pasted, notation = arrow_notation) %>%
                 paste_pref_suff(notation = arrow_notation),
               pasted)

  # Try with lists in the pref and suff arguments.
  expect_equal(paste_pref_suff(pref = "a", suff = "b", notation = arrow_notation), "a -> b")
  joined <- paste_pref_suff(pref = list("a", "c"), suff = list("b", "d"), notation = arrow_notation)
  expect_equal(joined, c("a -> b","c -> d"))

  # Try with 3 components in the lists
  expect_equal(paste_pref_suff(ps = list(pref = c("a", "b", "c"), suff = c("d", "e", "f"))),
               c("a -> d", "b -> e", "c -> f"))

  # Try with two lists.
  expect_equal(paste_pref_suff(ps = list(pref = c("a", "a"), suff = c("b", "b")),
                  notation = list(RCLabels::arrow_notation, RCLabels::arrow_notation)),
               c("a -> b", "a -> b"))
})


test_that("Pathological cases work for paste_pref_suff()", {
  expect_equal(paste_pref_suff(pref = "a", suff = c("c", "d"), notation = RCLabels::arrow_notation),
               c("a -> c", "a -> d"))
  # Trigger an error when notation is not a vector.
  # Do that by setting an attribute.
  my_bad_notation <- "d"
  attr(my_bad_notation, which = "my_bad_attr") <- "bad_attr"
  expect_error(paste_pref_suff(pref = "a", suff = "b", notation = my_bad_notation),
               regexp = "notation must be a list or a vector in paste_pref_suff")

  # Trigger an error when pref and suff have different lenghts but neither have length 1.
  expect_error(paste_pref_suff(pref = c("a", "b"), suff = c("c", "d", "e"), notation = RCLabels::arrow_notation),
               regexp = "lengths of pref")
})


test_that("paste_pref_suff() works when one is a list and the other is not", {
  res <- paste_pref_suff(pref = c("Biomass", "Biomass"), suff = "Resources", notation = RCLabels::from_notation)
  expect_equal(res, c("Biomass [from Resources]", "Biomass [from Resources]"))
  res2 <- paste_pref_suff(pref = "Biomass", suff = c("Resources", "Bogus"), notation = RCLabels::arrow_notation)
  expect_equal(res2, c("Biomass -> Resources", "Biomass -> Bogus"))
})


test_that("paste_pref_suff() works in a weird case", {
  notn <- make_list(RCLabels::of_notation, n = 3, lenx = 1)

  ps <- list(pref = c("a", "b", "b"),
             suff = c("c", "c", "c"))
  res <- paste_pref_suff(ps, notation = notn)
  expect_equal(res, c("a [of c]", "b [of c]", "b [of c]"))

  ps2 <- list(pref = c(pref = "a", pref = "b", pref = "b"),
              suff = c(suff = "c", suff = "c", suff = "c"))
  res2 <- paste_pref_suff(ps2, notation = notn)
  expect_equal(res2, c("a [of c]", "b [of c]", "b [of c]"))

  # Because names are stripped of sub-items,
  # it no longer matters what they are named.
  ps3 <- list(pref = c(pref = "a", pref = "b", pref = "b"),
              suff = c(pref = "c", pref = "c", pref = "c"))
  res3 <- paste_pref_suff(ps3, notation = notn)
  expect_equal(res3, c("a [of c]", "b [of c]", "b [of c]"))
})


test_that("paste_pref_suff() works with an empty data frame", {
  # Build an empty data frame
  df <- data.frame(pref = character(), suff = character()) %>%
    dplyr::mutate(
      pasted = paste_pref_suff(pref = .data[["pref"]], suff = .data[["suff"]], notation = RCLabels::from_notation)
    )
  expect_equal(nrow(df), 0)
  expect_equal(names(df), c("pref", "suff", "pasted"))
})


test_that("paste_pref_suff() fails with empty character array", {
  expect_equal(paste_pref_suff(pref = "a", suff = "", notation = RCLabels::from_notation), "a [from ]")
  expect_error(paste_pref_suff(pref = "a", suff = character(), notation = RCLabels::from_notation),
               regexp = "attempt to select less than one element in integerOneIndex")
})


test_that("paste_pref_suff() works with two empty character arrays", {
  # At this moment (18 Mar 2023),
  # using two character() objects returns list(), which doesn't match with other things,
  # like NA_character_.
  paste_pref_suff(pref = character(), suff = character(), notation = RCLabels::from_notation) |>
    expect_equal(character())
})


test_that("flip_pref_suff() works as expected", {
  # Try with inf_notation = FALSE
  expect_equal(flip_pref_suff("a -> b",
                              notation = arrow_notation,
                              inf_notation = FALSE), "b -> a")
  # Try without inference
  expect_equal(flip_pref_suff("a -> b", notation = arrow_notation), "b -> a")
  expect_equal(flip_pref_suff("a [b]", notation = bracket_notation), "b [a]")
  # Try with inference
  expect_equal(flip_pref_suff("a [b]"), "b [a]")
  # Try without inference.
  # This call essentially gives us flipped pref and suff for
  # all known notations.
  # This is nonsensical without supplying a notation.
  expect_error(flip_pref_suff("a [b]", inf_notation = FALSE))
  expect_equal(flip_pref_suff("a [b]", inf_notation = FALSE, notation = RCLabels::bracket_notation),
               "b [a]")

  # Make sure it works for lists
  expect_equal(flip_pref_suff(list("a -> b", "a -> b"), notation = arrow_notation, inf_notation = FALSE),
               c("b -> a", "b -> a"))
  expect_equal(flip_pref_suff(list("a -> b", "a -> b")),
               c("b -> a", "b -> a"))
  expect_equal(flip_pref_suff(list("a [b]", "a [b]"), notation = bracket_notation),
               c("b [a]", "b [a]"))

  # Try a case where prefix and suffix notation is different.
  nl <- notation_vec(pref_start = "(", pref_end = ")", suff_start = "[", suff_end = "]")
  expect_equal(flip_pref_suff("(a)[b]", notation = nl), "(b)[a]")

  # Try with nested suffixes
  expect_equal(flip_pref_suff("a [b [c]]", notation = bracket_notation), "b [c] [a]")

  # Try with inferred notation for different notations
  expect_equal(flip_pref_suff(list("a [b]", "a -> b")),
               c("b [a]", "b -> a"))
})


test_that("get_pref_suff() works as expected", {
  expect_equal(get_pref_suff("a -> b", which = "pref", notation = arrow_notation), c(pref = "a"))
  # Don't infer notation
  expect_equal(get_pref_suff("a -> b",
                             which = "pref",
                             inf_notation = FALSE,
                             notation = arrow_notation),
               c(pref = "a"))
  # Infer notation
  expect_equal(get_pref_suff("a -> b", which = "pref"), c(pref = "a"))
  expect_equal(get_pref_suff("a -> b", which = "suff", notation = arrow_notation), c(suff = "b"))

  expect_equal(get_pref_suff("a [b]", which = "suff"), c(suff = "b"))

  # Try with a character vector
  expect_equal(get_pref_suff(c("a -> b", "c -> d"), which = "pref"),
               c(pref = "a", pref = "c"))

  # Try with a list
  expect_equal(get_pref_suff(list("a -> b", "c -> d"), which = "pref"),
               c(pref = "a", pref = "c"))
  expect_equal(get_pref_suff(list("a -> b", "c -> d"), which = "suff"),
               c(suff = "b", suff = "d"))

  expect_equal(get_pref_suff(list("a [b]", "abcde"), which = "suff"), c(suff = "b", suff = ""))

  # Try degenerate cases
  expect_equal(get_pref_suff("abcde", which = "pref", notation = arrow_notation), c(pref = "abcde"))
  expect_equal(get_pref_suff("abcde", which = "suff", notation = arrow_notation), c(suff = ""))
  expect_equal(get_pref_suff(list("abcde", "fghij"), which = "pref", notation = arrow_notation),
               c(pref = "abcde", pref = "fghij"))
  expect_equal(get_pref_suff(list("abcde", "fghij"), which = "suff", notation = arrow_notation),
               c(suff = "", suff = ""))

  # Test in a data frame using mutate.
  df <- data.frame(v1 = c("a -> b", "c -> d"), v2 = c("e [f]", "g [h]"))
  res <- df %>%
    dplyr::mutate(
      # Keep the prefixes from the arrow notation column (v1)
      pref = get_pref_suff(v1, which = "pref", notation = arrow_notation),
      # Keep the suffixes from the bracket notation column (v2)
      suff = get_pref_suff(v2, which = "suff", notation = bracket_notation),
      # Keep the suffixes from the arrow notation column (v1), but specify bracket notation.
      # This should basically fail, because there are no suffixes.
      # Then, the entire string will be retained into the "fail" column.
      fail = get_pref_suff(v1, which = "suff", notation = bracket_notation)
    )
  expect_equal(res$pref, c(pref = "a", pref = "c"))
  expect_equal(res$pref[[1]], "a")
  expect_equal(res$pref[[2]], "c")
  expect_equal(res$suff, c(suff = "f", suff = "h"))
  expect_equal(res$suff[[1]], "f")
  expect_equal(res$suff[[2]], "h")
  expect_equal(res$fail, c(suff = "", suff = ""))
  expect_equal(res$fail[[1]], "")
  expect_equal(res$fail[[2]], "")
})


test_that("get_pref_suff() works when there is no prefix or suffix", {
  expect_equal(get_pref_suff("a", which = "pref", notation = arrow_notation), c(pref = "a"))
  expect_equal(get_pref_suff("a", which = "suff", notation = arrow_notation), c(suff = ""))
  expect_equal(get_pref_suff("a", which = "pref", notation = from_notation), c(pref = "a"))
  expect_equal(get_pref_suff("a", which = "suff", notation = arrow_notation), c(suff = ""))
})


test_that("get_pref_suff() preserves column type", {
  df <- data.frame(v1 = c("a -> b", "c -> d"))
  res <- df %>%
    dplyr::mutate(
      pref = get_pref_suff(v1, which = "pref", notation = arrow_notation)
    )
  expect_true(inherits(df$v1, what = "character"))
  expect_true(inherits(res$pref, what = "character"))
})


test_that("get_pref_suff() works with in notation", {
  expect_equal(get_pref_suff(c("a [in b]", "c [of d]"), which = "suff"),
               c(suff = "b", suff = "d"))
})


test_that("switch_notation() works as expected", {
  # Start with a degenerate case
  expect_equal(switch_notation("a", from = arrow_notation, to = bracket_notation), "a")
  expect_equal(switch_notation("a", from = bracket_notation, to = arrow_notation), "a")
  # Try with notation inference. Can't infer, so simply returns "a".
  expect_equal(switch_notation("a", to = bracket_notation), "a")

  # Now try "real" cases
  expect_equal(switch_notation("a -> b", from = arrow_notation, to = bracket_notation),
               "a [b]")
  expect_equal(switch_notation("a -> b", from = arrow_notation, to = bracket_notation, inf_notation = FALSE),
               "a [b]")
  expect_equal(switch_notation("a -> b", from = arrow_notation, to = bracket_notation, flip = TRUE),
               "b [a]")
  expect_equal(switch_notation("a [b]", from = bracket_notation, to = arrow_notation),
               "a -> b")
  expect_equal(switch_notation("a [b]", from = bracket_notation, to = arrow_notation, flip = TRUE),
               "b -> a")
  expect_equal(switch_notation("a [b]", from = bracket_notation, to = first_dot_notation),
               "a.b")
  expect_equal(switch_notation("a.b.c", from = first_dot_notation, to = arrow_notation),
               "a -> b.c")
  # Try when inferring notation
  expect_equal(switch_notation("a -> b", to = bracket_notation), "a [b]")
  expect_equal(switch_notation("a -> b", to = bracket_notation, flip = TRUE), "b [a]")
  expect_equal(switch_notation("a [b]", to = arrow_notation), "a -> b")
  expect_equal(switch_notation("a [b]", to = arrow_notation, flip = TRUE), "b -> a")
  expect_equal(switch_notation("a [b]", to = first_dot_notation), "a.b")
  expect_equal(switch_notation("a.b.c", to = arrow_notation), "a -> b.c")
  expect_equal(switch_notation("a.b.c", to = arrow_notation) %>%
                 switch_notation(from = first_dot_notation, to = arrow_notation),
               "a -> b -> c")

  # Try with a list
  expect_equal(switch_notation(list("a -> b", "c -> d"), to = bracket_notation),
               list("a [b]", "c [d]"))
  # Tell not to infer notation
  expect_error(switch_notation(list("a -> b", "c -> d"), to = bracket_notation, inf_notation = FALSE))
  expect_equal(switch_notation(list("a -> b", "c -> d"), from = arrow_notation, to = bracket_notation, inf_notation = FALSE),
               list("a [b]", "c [d]"))

  expect_equal(switch_notation(list("a -> b", "c -> d"), to = bracket_notation, flip = TRUE),
               list("b [a]", "d [c]"))
  expect_equal(switch_notation(list("a [b]", "c [d]"), from = bracket_notation, to = arrow_notation),
               list("a -> b", "c -> d"))

  # Try with 2 different notations
  expect_equal(switch_notation(list("a -> b", "c [of d]"), to = bracket_notation, flip = TRUE), list("b [a]", "d [c]"))
})


test_that("switch_notation() works in a data frame", {
  df <- data.frame(orig = c("a -> b", "c -> d"))
  switched <- df %>%
    dplyr::mutate(
      new = switch_notation(orig, from = arrow_notation, to = bracket_notation),
      new_inferred = switch_notation(orig, to = bracket_notation),
    )
  expect_equal(switched$new, list("a [b]", "c [d]"))
  expect_equal(switched$new_inferred, list("a [b]", "c [d]"))
})


test_that("infer_notation() works as expected for single x values", {
  expect_equal(infer_notation("abc", must_succeed = FALSE), NULL)
  expect_error(infer_notation("abc"), regexp = "Unable to infer notation for 'abc'")
  expect_equal(infer_notation("a -> b"), RCLabels::arrow_notation)
  expect_equal(infer_notation("a (b)"), RCLabels::paren_notation)
  expect_equal(infer_notation("a [b]"), RCLabels::bracket_notation)
  expect_equal(infer_notation("a [from b]", choose_most_specific = FALSE), RCLabels::bracket_notation)
  expect_equal(infer_notation("a [from b]", allow_multiple = TRUE, choose_most_specific = FALSE, retain_names = TRUE),
               list(bracket_notation = RCLabels::bracket_notation, from_notation = RCLabels::from_notation))
  expect_equal(infer_notation("a [of b]", allow_multiple = TRUE, choose_most_specific = FALSE, retain_names = TRUE),
               list(bracket_notation = RCLabels::bracket_notation, of_notation = RCLabels::of_notation))
  expect_equal(infer_notation("a [to b]", allow_multiple = TRUE, choose_most_specific = FALSE, retain_names = TRUE),
               list(bracket_notation = RCLabels::bracket_notation, to_notation = RCLabels::to_notation))
  expect_equal(infer_notation("a [-> b]", allow_multiple = TRUE, choose_most_specific = FALSE, retain_names = TRUE),
               list(bracket_notation = RCLabels::bracket_notation, bracket_arrow_notation = RCLabels::bracket_arrow_notation))
  expect_equal(infer_notation("a.b"), RCLabels::first_dot_notation)
  expect_equal(infer_notation("a.b.c.d"), RCLabels::first_dot_notation)
  # Try with requesting names
  expect_equal(infer_notation("a -> b", retain_names = TRUE), list(arrow_notation = RCLabels::arrow_notation))
  # Try with a restricted set of notations, not expecting a match.
  expect_equal(infer_notation("a -> b", notations = list(RCLabels::from_notation, RCLabels::bracket_arrow_notation), must_succeed = FALSE), NULL)
})


test_that("infer_notation() works as expected for multiple x values", {
  expect_equal(infer_notation(c("abcd", "efg"), must_succeed = FALSE), list(NULL, NULL))
  expect_equal(infer_notation(c("a -> b", "c -> d"), retain_names = FALSE),
               list(RCLabels::arrow_notation, RCLabels::arrow_notation))
  expect_equal(infer_notation(c("a -> b", "c -> d"), retain_names = TRUE),
               list(arrow_notation = RCLabels::arrow_notation, arrow_notation = RCLabels::arrow_notation))
  expect_equal(infer_notation(c("a -> b", "c [from d]"),
                              retain_names = TRUE,
                              allow_multiple = TRUE,
                              choose_most_specific = FALSE),
               list(list(arrow_notation = RCLabels::arrow_notation),
                    list(bracket_notation = RCLabels::bracket_notation,
                         from_notation = RCLabels::from_notation)))
  # Try when x is a list (as opposed to a vector)
  expect_equal(infer_notation(list("abcd", "efg"), must_succeed = FALSE), list(NULL, NULL))
  expect_equal(infer_notation(list(label1 = "a -> b", label2 = "c -> d"), retain_names = FALSE),
               list(RCLabels::arrow_notation, RCLabels::arrow_notation))
})


test_that("infer_notation() works as expected if choose_most_specific = TRUE", {
  expect_equal(infer_notation("a [from b]", choose_most_specific = TRUE), RCLabels::from_notation)
  expect_equal(infer_notation("a [from b]", retain_names = TRUE, choose_most_specific = TRUE),
               list(from_notation = RCLabels::from_notation))
  expect_equal(infer_notation("a [of b]", choose_most_specific = TRUE), RCLabels::of_notation)
  expect_equal(infer_notation("a [to b]", choose_most_specific = TRUE), RCLabels::to_notation)

  # Make sure choose_most_specific works for multiple labels in x
  expect_equal(infer_notation(c("a [to b]", "c [from d]"), choose_most_specific = TRUE),
               list(RCLabels::to_notation, RCLabels::from_notation))
  expect_equal(infer_notation(c("a [to b]", "c [from d]"), choose_most_specific = TRUE, retain_names = TRUE),
               list(to_notation = RCLabels::to_notation, from_notation = RCLabels::from_notation))
  # Test with ALL known notations
  expect_equal(infer_notation(c("a.b", "a - b", "a -> b", "a (b)", "a [b]", "a [from b]", "a [of b]", "a [to b]", "a [in b]", "a [-> b]"),
                              retain_names = TRUE), RCLabels::notations_list)
})


test_that("infer_notation() returns a list from a vector of x's and allow_multiple = TRUE", {
  # This should not return names on the notations, but at one time it did.
  expect_equal(infer_notation(c("a [from b]", "c [to d]"),
                              allow_multiple = TRUE,
                              retain_names = FALSE,
                              choose_most_specific = FALSE),
               list(list(RCLabels::bracket_notation, RCLabels::from_notation),
                    list(RCLabels::bracket_notation, RCLabels::to_notation)))
  # At one time, this returned a matrix!
  expect_equal(infer_notation(c("a [from b]", "c [to d]"),
                              allow_multiple = TRUE,
                              retain_names = TRUE,
                              choose_most_specific = FALSE),
               list(list(bracket_notation = RCLabels::bracket_notation, from_notation = RCLabels::from_notation),
                    list(bracket_notation = RCLabels::bracket_notation, to_notation = RCLabels::to_notation)))
})


test_that("infer_notation() works correctly with must_succeed = TRUE", {
  expect_error(infer_notation("abcde"), regexp = "Unable to infer notation for 'abcde'")
  expect_null(infer_notation("abcde", must_succeed = FALSE))
  expect_equal(infer_notation("a [of b]"), RCLabels::of_notation)
  expect_error(infer_notation(c("a [of b]", "abcde")), regexp = "Unable to infer notation for 'abcde'")
})


test_that("infer_notation() works with 'a [from b]' and must_succeed = TRUE", {
  expect_equal(infer_notation("a [from b]", choose_most_specific = FALSE, must_succeed = TRUE), RCLabels::bracket_notation)
})


test_that("infer_notation() returns notations when inf_notation = FALSE", {
  expect_equal(infer_notation("a -> b", inf_notation = FALSE), RCLabels::notations_list)
  expect_equal(infer_notation("a -> b", inf_notation = FALSE), RCLabels::notations_list)
})


test_that("infer_notation() correctly flags a malformed label", {
  expect_error(infer_notation("] [from b"),
               regexp = "Unable to infer notation for ")
})


test_that("infer_notation() identifies a pathological case", {
  expect_equal(infer_notation("a [from b]", allow_multiple = FALSE, choose_most_specific = FALSE, must_succeed = FALSE),
               RCLabels::bracket_notation)
})


test_that("inference works with 'HTH.400.C'", {
  expect_equal(infer_notation("HTH.400.C"), RCLabels::first_dot_notation)
  expect_equal(get_piece("HTH.400.C", piece = "noun"), c(noun = "HTH"))
})

