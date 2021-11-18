test_that("get_noun() works as expected", {
  expect_equal(get_noun("a [b]", bracket_notation), list("a"))

  expect_equal(get_noun(c("a [b]", "c [d]"), bracket_notation), list("a", "c"))
})
