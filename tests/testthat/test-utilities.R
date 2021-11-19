test_that("make_pattern() works as expected", {
  expect_equal(make_or_pattern(c("a", "b")), "^a$|^b$")
  expect_equal(make_or_pattern(c("a", "b"), pattern_type = "exact"), "^a$|^b$")
  expect_equal(make_or_pattern(c("a", "b"), pattern_type = "leading"), "^a|^b")
  expect_equal(make_or_pattern(c("a", "b"), pattern_type = "trailing"), "a$|b$")
  expect_equal(make_or_pattern(c("a", "b"), pattern_type = "anywhere"), "a|b")
  expect_equal(make_or_pattern(c("a", "b"), pattern_type = "literal"), c("a", "b"))

  expect_equal(make_or_pattern(c("a", "b", "c")), "^a$|^b$|^c$")
})
