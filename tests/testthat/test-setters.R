test_that("set_nouns() works as expected", {
  label <- "a [of b in c]"
  expect_equal(set_nouns(label, "d"), "d [of b in c]")

  labels <- c("a [of b in c]", "d [of e in USA]")
  expect_equal(set_nouns(labels, c("a_plus", "g")),
               c("a_plus [of b in c]", "g [of e in USA]"))
})
