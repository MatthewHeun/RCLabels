test_that("get_noun() works as expected", {
  expect_equal(get_noun("a [b]"), list("a"))
  expect_equal(get_noun(c("a [b]", "c [d]")), list("a", "c"))
  expect_equal(get_noun(list("a [b]", "c [d]")), list("a", "c"))
})


test_that("suff_to_pp() works as expected", {
  expect_equal(extract_pp("a [of b in c]", bracket_notation), list("of b", "in c"))
})


test_that("get_pp() works as expected", {
  expect_equal(extract_pp("a [in b]"), list("in b"))
})
