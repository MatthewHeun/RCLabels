test_that("modify_nouns() works as expected", {
  label <- "a [of b in c]"
  expect_equal(modify_nouns(label, "d"), "d [of b in c]")

  labels <- c("a [of b in c]", "d [of e in USA]")
  expect_equal(modify_nouns(labels, c("a_plus", "g")),
               c("a_plus [of b in c]", "g [of e in USA]"))

  # Make sure it works in a data frame.
  df <- tibble::tibble(labels = c("a [of b in c]", "d [of e in USA]"),
                       new_nouns = c("first_noun", "second_noun"))
  res <- df |>
    dplyr::mutate(
      with_new_nouns = modify_nouns(labels, new_nouns = new_nouns)
    )
  expect_equal(res$with_new_nouns[[1]], "first_noun [of b in c]")
  expect_equal(res$with_new_nouns[[2]], "second_noun [of e in USA]")
})


test_that("modify_nouns() fails when the shape of new_nouns is wrong", {
  expect_error(modify_nouns("a [of b in c]", c("d", "e")),
               "The number of labels must equal the number of new nouns in set_nouns")
})


test_that("modify_label_pieces() works as expected with single strings", {
  label <- "a [of b in c]"
  expect_equal(modify_label_pieces(label, piece = "noun", mod_map = list(new_noun = c("a", "b"))),
               "new_noun [of b in c]")
  expect_equal(modify_label_pieces(label, piece = "of", mod_map = list(new_of = c("a", "b"))),
               "a [of new_of in c]")
  # This one should result in no replacements,
  # because "c" is not %in% c("a", "b").
  expect_equal(modify_label_pieces(label, piece = "in", mod_map = list(USA = c("a", "b"))),
               "a [of b in c]")
  # Now, let's make it work.
  expect_equal(modify_label_pieces(label, piece = "in", mod_map = list(USA = c("a", "b", "c"))),
               "a [of b in USA]")
})


test_that("modify_label_pieces() works with vectors, lists, and in data frames", {
  # Try with a vector
  labs <- c("a [of b in c]", "d [-> e in f]")
  expect_equal(modify_label_pieces(labs, piece = "noun", mod_map = list(new_noun = c("d", "e"))),
               c("a [of b in c]", "new_noun [-> e in f]"))
  # Try with a list
  labs2 <- list("a [of b in c]", "d [-> e in f]")
  expect_equal(modify_label_pieces(labs2, piece = "->", mod_map = list(`new_->` = c("d", "e"))),
               c("a [of b in c]", "d [-> new_-> in f]"))

  # Try in a data frame
  df <- tibble::tibble(labs = c("a [of b in c]", "d [of e in f]"))
  res <- df |>
    dplyr::mutate(
      mod1 = modify_label_pieces(labs, piece = "noun", mod_map = list(new_a = c("a", "b", "c")))
    )
  expect_equal(res$mod1, c("new_a [of b in c]", "d [of e in f]"))
})


test_that("modify_label_pieces() works with 2x piece", {
  labs <- c("a [of b in c]", "d [-> e in f]")
  expect_equal(modify_label_pieces(labs,
                                   piece = c("noun", "in"),
                                   mod_map = list(new_noun = c("a", "b", "c"),
                                                    new_in   = c("c", "f"))),
               c("new_noun [of b in new_in]", "d [-> e in new_in]"))
})


test_that("modify_label_pieces() works with 2x mod_map", {
  labs <- c("a [of b in c]", "d [-> e in f]")
  expect_equal(modify_label_pieces(labs,
                                   piece = "noun",
                                   mod_map = list(new_noun1 = c("a", "b", "c"),
                                                    new_noun2 = c("d", "e", "f"))),
               c("new_noun1 [of b in c]", "new_noun2 [-> e in f]"))
})


test_that("remove_pp() works as expected", {
  labs <- c("a [of b in c]", "d [-> e in f]")
  expect_equal(remove_label_pieces(labs, pieces_to_remove = "of"),
               c("a [in c]", "d [-> e in f]"))
  expect_equal(remove_label_pieces(labs, pieces_to_remove = c("of", "->")),
               c("a [in c]", "d [in f]"))
  expect_equal(remove_label_pieces(labs, pieces_to_remove = c("in", "into")),
               c("a [of b]", "d [-> e]"))
  expect_equal(remove_label_pieces(labs, pieces_to_remove = c("of", "in")),
               c("a [ ]", "d [-> e]"))
})
