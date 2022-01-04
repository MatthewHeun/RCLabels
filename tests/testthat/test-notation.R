

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


test_that("from_notation works is correct", {
  fn <- from_notation
  expect_equal(fn[["pref_start"]], "")
  expect_equal(fn[["pref_end"]], " [from ")
  expect_equal(fn[["suff_start"]], " [from ")
  expect_equal(fn[["suff_end"]], "]")
})


test_that("of_notation works is correct", {
  on <- of_notation
  expect_equal(on[["pref_start"]], "")
  expect_equal(on[["pref_end"]], " [of ")
  expect_equal(on[["suff_start"]], " [of ")
  expect_equal(on[["suff_end"]], "]")
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
  expect_equal(RCLabels:::split_pref_suff("a [b]", bracket_notation), list(pref = "a", suff = "b"))

  expect_equal(RCLabels:::split_pref_suff(c("a [b]", "c [d]"), bracket_notation),
               list(pref = c("a", "c"), suff = c("b", "d")))

  expect_equal(RCLabels:::split_pref_suff(list("a [b]", "c [d]"), bracket_notation),
               list(pref = c("a", "c"), suff = c("b", "d")))

  expect_equal(RCLabels:::split_pref_suff(c("a [b]", "c [d]", "e [f]", "g [h]"), bracket_notation),
               list(pref = c("a", "c", "e", "g"),
                    suff = c("b", "d", "f", "h")))
})


test_that("split_pref_suff() works with full notation", {
  note <- notation_vec(pref_start = "(", pref_end = ")" , suff_start = "(", suff_end = ")")
  expect_equal(RCLabels:::split_pref_suff(c("(a)(b)", "(c)(d)"), note),
               list(pref = c("a", "c"),
                    suff = c("b", "d")))
})


test_that("split_pref_suff() works in a data frame", {
  df <- data.frame(donottouch = c(1, 2, 3), orig = c("a -> b", "c -> d", "e -> f"))
  split <- df |>
    dplyr::mutate(
      split = RCLabels:::split_pref_suff(orig, notation = arrow_notation, transpose = TRUE)
    )
  expect_equal(split$split, list(list(pref = "a", suff = "b"),
                                    list(pref = "c", suff = "d"),
                                    list(pref = "e", suff = "f")))
})


test_that("paste_pref_suff() works properly", {
  ps <- c(pref = "a", suff = "b")
  expect_equal(paste_pref_suff(ps, notation = arrow_notation), "a -> b")
  # Make sure that they are the inverse of each other,
  # except for converting to a list.
  expect_equal(paste_pref_suff(ps, notation = arrow_notation) |>
                 split_pref_suff(notation = arrow_notation), as.list(ps))
  # Try with paren notation list
  expect_equal(paste_pref_suff(ps, notation = bracket_notation) |>
                 split_pref_suff(notation = bracket_notation), as.list(ps))
  # Try with a wacky notation list
  amp_nl <- notation_vec(sep = "&&&&&&&&")
  expect_equal(paste_pref_suff(ps, notation = amp_nl) |>
                 split_pref_suff(notation = amp_nl), as.list(ps))
  paren_nl <- notation_vec(pref_start = "(", pref_end = ")",
                           suff_start = "(", suff_end = ")")
  expect_equal(paste_pref_suff(ps, notation = paren_nl) |>
                 split_pref_suff(notation = paren_nl), as.list(ps))
  # Try to join lists
  expect_equal(paste_pref_suff(list(list(pref = "a", suff = "b"), list(pref = "c", suff = "d"))),
               list("a -> b", "c -> d"))
  # Try to split then join lists
  joined <- c("a -> b", "c -> d")
  expect_equal(split_pref_suff(joined, notation = arrow_notation) |>
                 paste_pref_suff(notation = arrow_notation),
               joined)

  # Try with lists in the pref and suff arguments.
  expect_equal(paste_pref_suff(pref = "a", suff = "b", notation = arrow_notation), "a -> b")
  joined <- paste_pref_suff(pref = list("a", "c"), suff = list("b", "d"), notation = arrow_notation)
  expect_equal(joined, c("a -> b","c -> d"))

  # Try with 3 components in the lists
  expect_equal(paste_pref_suff(ps = list(pref = c("a", "b", "c"), suff = c("d", "e", "f"))),
               c("a -> d", "b -> e", "c -> f"))
})


test_that("flip_pref_suff() works as expected", {
  expect_equal(flip_pref_suff("a -> b", notation = arrow_notation), "b -> a")
  expect_equal(flip_pref_suff("a [b]", notation = bracket_notation), "b [a]")

  # Make sure it works for lists
  expect_equal(flip_pref_suff(list("a -> b", "a -> b"), notation = arrow_notation),
               c("b -> a", "b -> a"))
  expect_equal(flip_pref_suff(list("a [b]", "a [b]"), notation = bracket_notation),
               c("b [a]", "b [a]"))

  # Try a case where prefix and suffix notation is different.
  nl <- notation_vec(pref_start = "(", pref_end = ")", suff_start = "[", suff_end = "]")
  expect_equal(flip_pref_suff("(a)[b]", notation = nl), "(b)[a]")

  # Try with nested suffixes
  expect_equal(flip_pref_suff("a [b [c]]", notation = bracket_notation), "b [c] [a]")
})


test_that("get_pref_suff() works as expected", {
  expect_equal(get_pref_suff("a -> b", which = "pref", notation = arrow_notation), c(pref = "a"))
  expect_equal(get_pref_suff("a -> b", which = "suff", notation = arrow_notation), c(suff = "b"))

  expect_equal(get_pref_suff("a [b]", which = "suff", notation = bracket_notation), c(suff = "b"))

  # Try with a character vector
  expect_equal(get_pref_suff(c("a -> b", "c -> d"), which = "pref", notation = arrow_notation),
               c(pref = "a", pref = "c"))

  # Try with a list
  expect_equal(get_pref_suff(list("a -> b", "c -> d"), which = "pref", notation = arrow_notation),
               c(pref = "a", pref = "c"))
  expect_equal(get_pref_suff(list("a -> b", "c -> d"), which = "suff", notation = arrow_notation),
               c(suff = "b", suff = "d"))

  expect_equal(get_pref_suff(list("a [b]", "abcde"), which = "suff", notation = bracket_notation),
               c(suff = "b", suff = ""))

  # Try degenerate cases
  expect_equal(get_pref_suff("abcde", which = "pref", notation = arrow_notation), c(pref = "abcde"))
  expect_equal(get_pref_suff("abcde", which = "suff", notation = arrow_notation), c(suff = ""))
  expect_equal(get_pref_suff(list("abcde", "fghij"), which = "pref", notation = arrow_notation),
               c(pref = "abcde", pref = "fghij"))
  expect_equal(get_pref_suff(list("abcde", "fghij"), which = "suff", notation = arrow_notation),
               c(suff = "", suff = ""))

  # Test in a data frame using mutate.
  df <- data.frame(v1 = c("a -> b", "c -> d"), v2 = c("e [f]", "g [h]"))
  res <- df |>
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
  res <- df |>
    dplyr::mutate(
      pref = get_pref_suff(v1, which = "pref", notation = arrow_notation)
    )
  expect_true(inherits(df$v1, what = "character"))
  expect_true(inherits(res$pref, what = "character"))
})


test_that("switch_notation() works as expected", {
  # Start with a degenerate case
  expect_equal(switch_notation("a", from = arrow_notation, to = bracket_notation), "a")
  expect_equal(switch_notation("a", from = bracket_notation, to = arrow_notation), "a")

  # Now try "real" cases
  expect_equal(switch_notation("a -> b", from = arrow_notation, to = bracket_notation),
               "a [b]")
  expect_equal(switch_notation("a -> b", from = arrow_notation, to = bracket_notation, flip = TRUE),
               "b [a]")
  expect_equal(switch_notation("a [b]", from = bracket_notation, to = arrow_notation),
               "a -> b")
  expect_equal(switch_notation("a [b]", from = bracket_notation, to = arrow_notation, flip = TRUE),
               "b -> a")

  # Try with a list
  expect_equal(switch_notation(list("a -> b", "c -> d"), from = arrow_notation, to = bracket_notation),
               list("a [b]", "c [d]"))
  expect_equal(switch_notation(list("a -> b", "c -> d"), from = arrow_notation, to = bracket_notation, flip = TRUE),
               list("b [a]", "d [c]"))
  expect_equal(switch_notation(list("a [b]", "c [d]"), from = bracket_notation, to = arrow_notation),
               list("a -> b", "c -> d"))
})


test_that("switch_notation() works in a data frame", {
  df <- data.frame(orig = c("a -> b", "c -> d"))
  switched <- df |>
    dplyr::mutate(
      new = switch_notation(orig, from = arrow_notation, to = bracket_notation)
    )
  expect_equal(switched$new, list("a [b]", "c [d]"))
})

