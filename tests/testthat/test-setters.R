test_that("set_nouns() works as expected", {
  label <- "a [of b in c]"
  expect_equal(set_nouns(label, "d"), "d [of b in c]")

  labels <- c("a [of b in c]", "d [of e in USA]")
  expect_equal(set_nouns(labels, c("a_plus", "g")),
               c("a_plus [of b in c]", "g [of e in USA]"))
})


test_that("set_nouns() fails when the shape of new_nouns is wrong", {
  expect_error(set_nouns("a [of b in c]", c("d", "e")),
               "The number of labels must equal the number of new nouns in set_nouns")
})
