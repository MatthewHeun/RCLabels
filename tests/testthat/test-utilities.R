test_that("make_pattern() works as expected", {
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
