test_that("make_or_pattern() works as expected", {
  expect_equal(make_or_pattern(c("a", "b")), "^a$|^b$")
  expect_equal(make_or_pattern(c("a", "b"), pattern_type = "exact"), "^a$|^b$")
  expect_equal(make_or_pattern(c("a", "b"), pattern_type = "leading"), "^a|^b")
  expect_equal(make_or_pattern(c("a", "b"), pattern_type = "trailing"), "a$|b$")
  expect_equal(make_or_pattern(c("a", "b"), pattern_type = "anywhere"), "a|b")
  expect_equal(make_or_pattern(c("a", "b"), pattern_type = "literal"), c("a", "b"))

  expect_equal(make_or_pattern(c("a", "b", "c")), "^a$|^b$|^c$")
})


test_that('prepositions make good "or" patterns', {
  preps_pattern <- make_or_pattern(RCLabels::prepositions_list, pattern_type = "anywhere")
  res <- gregexpr(preps_pattern, c("a [of b]", "cat [from d]", "haunted [-> house]"))
  expect_equal(res, list(4, 6, 10), ignore_attr = TRUE)
  expect_equal(attr(res[[1]], which = "match.length"), 2)
  expect_equal(attr(res[[2]], which = "match.length"), 4)
  expect_equal(attr(res[[3]], which = "match.length"), 2)
})


test_that("match_by_pattern() works as expected for string matches", {
  labels <- c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]")
  # Simple matching
  expect_equal(match_by_pattern(labels,
                                regex_pattern = "Production"),
               c(TRUE, FALSE, FALSE))
  expect_equal(match_by_pattern(labels,
                                regex_pattern = "Coal"),
               c(FALSE, TRUE, FALSE))
  expect_equal(match_by_pattern(labels,
                                regex_pattern = "USA"),
               c(FALSE, FALSE, TRUE))

  # Check word positions
  expect_equal(match_by_pattern(labels,
                                regex_pattern = "^Production"),
               c(TRUE, FALSE, FALSE))
  # This should fail, because Production is at the start of the first string,
  # not the end of a string.
  expect_equal(match_by_pattern(labels,
                                regex_pattern = "Production$"),
               c(FALSE, FALSE, FALSE))
})


test_that("match_by_pattern() works for prefixes and suffixes", {
  labels <- c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]")

  # This should give an error,
  # because we should have only "pref" or "suff", not both.
  expect_error(match_by_pattern(labels,
                                regex_pattern = "Production",
                                pieces = c("pref", "to")),
               'If pieces contains "pref" or "suff", its length')

  # This should work, because "Production" is in the prefix.
  expect_equal(match_by_pattern(labels,
                                regex_pattern = "Production",
                                pieces = "pref"),
               c(TRUE, FALSE, FALSE))

  # This should fail, because "Production" is not in the suffix.
  expect_equal(match_by_pattern(labels,
                                regex_pattern = "Production",
                                pieces = "suff"),
               c(FALSE, FALSE, FALSE))
})


test_that("match_by_pattern() works for nouns and prepositions", {
  labels <- c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]")
  # This should work, because "Production" is a noun.
  expect_equal(match_by_pattern(labels,
                                regex_pattern = "Production",
                                pieces = "noun"),
               c(TRUE, FALSE, FALSE))

  # This won't work, because "Production" is a noun, not in a prepositional phrase.
  expect_equal(match_by_pattern(labels,
                                regex_pattern = "Production",
                                pieces = "in"),
               c(FALSE, FALSE, FALSE))

  # Try a preposition
  expect_equal(match_by_pattern(labels,
                                regex_pattern = make_or_pattern(c("c", "f")),
                                pieces = "in"),
               c(TRUE, TRUE, FALSE))

  # This should match only the USA one.
  expect_equal(match_by_pattern(labels,
                                regex_pattern = make_or_pattern(c("b", "Coal", "USA")),
                                pieces = "in"),
               c(FALSE, FALSE, TRUE))

  # This should match all labels.
  expect_equal(match_by_pattern(labels,
                                regex_pattern = make_or_pattern(c("b", "Coal", "USA")),
                                pieces = c("of", "in")),
               c(TRUE, TRUE, TRUE))
})


test_that("match_by_pattern() works for degenerate case", {
  labels <- c("Production [of b to GBR in c]", "d [of Coal in f]", "g [of h in USA]")
  # Try a situation that will return non-square results.
  expect_equal(match_by_pattern(labels,
                                regex_pattern = make_or_pattern(c("b", "Coal", "GBR", "USA")),
                                pieces = c("noun", "of", "in", "to"),
                                prepositions = c("of", "to", "in")),
               c(TRUE, TRUE, TRUE))
})


test_that("replace_by_pattern() works as expected", {
  labels <- c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]")
  expect_equal(replace_by_pattern(labels,
                                  regex_pattern = "Production",
                                  replacement = "Manufacture"),
               c("Manufacture [of b in c]", "d [of Coal in f]", "g [of h in USA]"))
  expect_equal(replace_by_pattern(labels,
                                  regex_pattern = "Coal",
                                  replacement = "Oil"),
               c("Production [of b in c]", "d [of Oil in f]", "g [of h in USA]"))
  expect_equal(replace_by_pattern(labels,
                                  regex_pattern = "USA",
                                  replacement = "GHA"),
               c("Production [of b in c]", "d [of Coal in f]", "g [of h in GHA]"))
})


test_that("replace_by_pattern() works as expected with prefixes and suffixes", {
  labels <- c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]")
  expect_equal(replace_by_pattern(labels,
                                  regex_pattern = "Production",
                                  replacement = "Manufacture",
                                  pieces = "pref"),
               c("Manufacture [of b in c]", "d [of Coal in f]", "g [of h in USA]"))

  expect_equal(replace_by_pattern(labels,
                                  regex_pattern = "Coa",
                                  replacement = "Bow",
                                  pieces = "suff"),
               c("Production [of b in c]", "d [of Bowl in f]", "g [of h in USA]"))

  # Nothing should change, because USA is in the suffix.
  expect_equal(replace_by_pattern(labels,
                                  regex_pattern = "SA",
                                  replacement = "SSR",
                                  pieces = "pref"),
               c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]"))

  # Now USA --> USSR, because USA is in the suffix.
  expect_equal(replace_by_pattern(labels,
                                  regex_pattern = "SA",
                                  replacement = "SSR",
                                  pieces = "suff"),
               c("Production [of b in c]", "d [of Coal in f]", "g [of h in USSR]"))

  # This should throw an error, because only "pref" or "suff" can be specified.
  expect_error(replace_by_pattern(labels,
                                  regex_pattern = "SA",
                                  replacement = "SSR",
                                  pieces = c("pref", "suff")),
               'If pieces contains "pref" or "suff", its length must be 1. Length was 2.')

  # This should throw an error, because only "pref" or "suff" can be specified.
  expect_error(replace_by_pattern(labels,
                                  regex_pattern = "SA",
                                  replacement = "SSR",
                                  pieces = c("pref", "bogus", "42")),
               'If pieces contains "pref" or "suff", its length must be 1. Length was 3.')
})


test_that("replace_by_pattern() works for nouns and prepositions", {
  labels <- c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]")
  expect_equal(replace_by_pattern(labels,
                                  regex_pattern = "Production",
                                  replacement = "Manufacture",
                                  pieces = "noun"),
               c("Manufacture [of b in c]", "d [of Coal in f]", "g [of h in USA]"))

  expect_equal(replace_by_pattern(labels,
                                  regex_pattern = "^Pro",
                                  replacement = "Con",
                                  pieces = "noun"),
               c("Conduction [of b in c]", "d [of Coal in f]", "g [of h in USA]"))
  # Won't match: wrong side of string.
  expect_equal(replace_by_pattern(labels,
                                  regex_pattern = "Pro$",
                                  replacement = "Con",
                                  pieces = "noun"),
               c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]"))
  # No change, because "Production" is a noun.
  expect_equal(replace_by_pattern(labels,
                                  regex_pattern = "Production",
                                  replacement = "Manufacture",
                                  pieces = "of"),
               c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]"))
  # Now try with "of".
  expect_equal(replace_by_pattern(labels,
                                  regex_pattern = "Coal",
                                  replacement = "Oil",
                                  pieces = "of"),
               c("Production [of b in c]", "d [of Oil in f]", "g [of h in USA]"))
  # No change, because "Coal" is not "in" anything.
  expect_equal(replace_by_pattern(labels,
                                  regex_pattern = "Coal",
                                  replacement = "Oil",
                                  pieces = "in"),
               c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]"))

  # Now try in "in".
  expect_equal(replace_by_pattern(labels,
                                  regex_pattern = "USA",
                                  replacement = "GBR",
                                  pieces = "in"),
               c("Production [of b in c]", "d [of Coal in f]", "g [of h in GBR]"))
  # Replace at end of word
  expect_equal(replace_by_pattern(labels,
                                  regex_pattern = "A$",
                                  replacement = "upercalifragilisticexpialidocious",
                                  pieces = "in"),
               c("Production [of b in c]", "d [of Coal in f]", "g [of h in USupercalifragilisticexpialidocious]"))
})


test_that("make_list() works as expected", {
  m <- matrix(c(1:6), nrow = 3, dimnames = list(c("r1", "r2", "r3"), c("c2", "c1")))

  expect_equal(make_list(m, n = 1), list(m))
  expect_equal(make_list(m, n = 2), list(m, m))
  expect_equal(make_list(m, n = 5), list(m, m, m, m, m))
  l1 <- list(c(1,2), c(3,4))
  # Expect c(1,2), c(3,4), c(1,2), c(3,4)
  expect_equal(make_list(l1, n = 4), c(l1, l1))
  # Expect [c(1,2), c(3,4)], [c(1,2), c(3,4)], [c(1,2), c(3,4)], [c(1,2), c(3,4)]
  expect_equal(make_list(l1, n = 4, lenx = 1), list(l1, l1, l1, l1))
  # Expect a warning, because length isn't a multiple
  expect_warning(make_list(l1, n = 3), "n not evenly divisible by length\\(x\\)")

  m1 <- matrix(1:4, nrow = 2)
  m2 <- m + 100
  l2 <- list(m1, m2)
  expect_equal(make_list(l2, n = 4), c(l2, l2))
  expect_warning(make_list(l2, n = 1), "n not evenly divisible by length\\(x\\)")
  expect_warning(make_list(l2, n = 5), "n not evenly divisible by length\\(x\\)")

  l3 <- list(c("r10", "r11"), c("c10", "c11"))
  expect_equal(make_list(l3, n = 2), l3) # Confused by x being a list
  expect_equal(make_list(l3, n = 2, lenx = 1), list(l3, l3)) # Fix by setting lenx = 1

  margin <- c(1, 2)
  # This approach spreads 1, 2 to the two items in the list.
  expect_equal(make_list(margin, n = 2), list(1, 2))
  # This approach considers c(1,2) to be the item to be repeated.
  expect_equal(make_list(margin, n = 2, lenx = 1), list(c(1,2), c(1,2)))
})

