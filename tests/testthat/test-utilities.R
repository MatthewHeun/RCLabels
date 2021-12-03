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
  preps_pattern <- make_or_pattern(RCLabels::prepositions, pattern_type = "anywhere")
  res <- gregexpr(preps_pattern, c("a [of b]", "cat [from d]", "haunted [-> house]"))
  expect_equal(res, list(4, 6, 10), ignore_attr = TRUE)
  expect_equal(attr(res[[1]], which = "match.length"), 2)
  expect_equal(attr(res[[2]], which = "match.length"), 4)
  expect_equal(attr(res[[3]], which = "match.length"), 2)
})


test_that("match_pattern() works as expected for string matches", {
  labels <- c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]")
  # Simple matching
  expect_equal(match_pattern(labels,
                             regex_pattern = "Production"),
               c(TRUE, FALSE, FALSE))
  expect_equal(match_pattern(labels,
                             regex_pattern = "Coal"),
               c(FALSE, TRUE, FALSE))
  expect_equal(match_pattern(labels,
                             regex_pattern = "USA"),
               c(FALSE, FALSE, TRUE))

  # Check word positions
  expect_equal(match_pattern(labels,
                             regex_pattern = "^Production"),
               c(TRUE, FALSE, FALSE))
  # This should fail, because Production is at the start of the first string,
  # not the end of a string.
  expect_equal(match_pattern(labels,
                             regex_pattern = "Production$"),
               c(FALSE, FALSE, FALSE))
})


test_that("match_pattern() works for prefixes and suffixes", {
  labels <- c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]")
  # This should work, because "Production" is in the prefix.
  expect_equal(match_pattern(labels,
                             regex_pattern = "Production",
                             pieces = "pref"),
               c(TRUE, FALSE, FALSE))

  # This should fail, because "Production" is not in the suffix.
  expect_equal(match_pattern(labels,
                             regex_pattern = "Production",
                             pieces = "suff"),
               c(FALSE, FALSE, FALSE))
})


test_that("match_pattern() works for nouns and prepositions", {
  labels <- c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]")
  # This should work, because "Production" is a noun.
  expect_equal(match_pattern(labels,
                             regex_pattern = "Production",
                             pieces = "noun"),
               c(TRUE, FALSE, FALSE))

  # This won't work, because "Production" is a noun, not in a prepositional phrase.
  expect_equal(match_pattern(labels,
                             regex_pattern = "Production",
                             pieces = "in"),
               c(FALSE, FALSE, FALSE))


})
