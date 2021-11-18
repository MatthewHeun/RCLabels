test_that("get_noun() works as expected", {
  expect_equal(get_noun("a [b]"), list("a"))
  expect_equal(get_noun(c("a [b]", "c [d]")), list("a", "c"))
  expect_equal(get_noun(list("a [b]", "c [d]")), list("a", "c"))
})
