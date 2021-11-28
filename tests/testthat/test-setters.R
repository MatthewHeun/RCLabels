test_that("set_nouns() works as expected", {
  label <- "a [of b in c]"
  expect_equal(set_nouns(label, "d"), "d [of b in c]")

  labels <- c("a [of b in c]", "d [of e in USA]")
  expect_equal(set_nouns(labels, c("a_plus", "g")),
               c("a_plus [of b in c]", "g [of e in USA]"))

  # Make sure it works in a data frame.
  df <- tibble::tibble(labels = c("a [of b in c]", "d [of e in USA]"),
                       new_nouns = c("first_noun", "second_noun"))
  res <- df |>
    dplyr::mutate(
      with_new_nouns = set_nouns(labels, new_nouns = new_nouns)
    )
  expect_equal(res$with_new_nouns[[1]], "first_noun [of b in c]")
  expect_equal(res$with_new_nouns[[2]], "second_noun [of e in USA]")
})


test_that("set_nouns() fails when the shape of new_nouns is wrong", {
  expect_error(set_nouns("a [of b in c]", c("d", "e")),
               "The number of labels must equal the number of new nouns in set_nouns")
})


test_that("modify_label_pieces() works as expected with single strings", {
  label <- "a [of b in c]"
  expect_equal(modify_label_pieces(label, piece = "noun", label_map = list(new_noun = c("a", "b"))),
               "new_noun [of b in c]")
  expect_equal(modify_label_pieces(label, piece = "of", label_map = list(new_of = c("a", "b"))),
               "a [of new_of in c]")
  # This one should result in no replacements,
  # because "c" is not %in% c("a", "b").
  expect_equal(modify_label_pieces(label, piece = "in", label_map = list(USA = c("a", "b"))),
               "a [of b in c]")
  # Now, let's make it work.
  expect_equal(modify_label_pieces(label, piece = "in", label_map = list(USA = c("a", "b", "c"))),
               "a [of b in USA]")
})


test_that("modify_label_pieces() works with vectors, lists, and in data frames", {
  labs <- c("a [of b in c]", "d [-> e in f]")
  expect_equal(modify_label_pieces(labs, piece = "noun", label_map = list(new_noun = c("d", "e"))),
               c("a [of b in c]", "new_noun [-> e in f]"))
  labs2 <- list("a [of b in c]", "d [-> e in f]")
  expect_equal(modify_label_pieces(labs2, piece = "->", label_map = list(`new_->` = c("d", "e"))),
               c("a [of b in c]", "d [-> new_-> in f]"))

})
