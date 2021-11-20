test_that("get_noun() works as expected", {
  expect_equal(get_noun("a [b]"), list("a"))
  expect_equal(get_noun(c("a [b]", "c [d]")), list("a", "c"))
  expect_equal(get_noun(list("a [b]", "c [d]")), list("a", "c"))
})



test_that("separate_pp() works as expected", {
  expect_equal(separate_pp(c("a [of b in c]", "d [of e into f]"), bracket_notation),
               list(list("of b", "in c"), list("of e", "into f")))
  expect_equal(separate_pp(list("a [of b in c]", "d [of e into f]"), bracket_notation),
               list(list("of b", "in c"), list("of e", "into f")))



  expect_equal(separate_pp("a [in b]"), list(list("in b")))
  expect_equal(separate_pp(c("a [in b]")), list(list("in b")))
  expect_equal(separate_pp(list("a [in b]")), list(list("in b")))


})
