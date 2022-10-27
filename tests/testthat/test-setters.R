test_that("modify_nouns() works as expected", {
  # Specify the notation
  expect_equal(modify_nouns("a [of b in c]", new_nouns = "d", notation = RCLabels::bracket_notation), "d [of b in c]")
  # Don't infer the notation
  expect_equal(modify_nouns("a [of b in c]", new_nouns = "d", inf_notation = FALSE, notation = RCLabels::bracket_notation), "d [of b in c]")
  # Potentially cause problems.
  expect_error(modify_nouns("a [of b in c]", new_nouns = "d", inf_notation = FALSE))
  # Infer notation
  expect_equal(modify_nouns("a [of b in c]", new_nouns = "d"), "d [of b in c]")

  labels <- c("a [of b in c]", "d [of e in USA]")
  # Specify the notation
  expect_equal(modify_nouns(labels, new_nouns = c("a_plus", "g"), notation = RCLabels::bracket_notation),
               c("a_plus [of b in c]", "g [of e in USA]"))
  # Infer notation
  expect_equal(modify_nouns(labels, new_nouns = c("a_plus", "g")),
               c("a_plus [of b in c]", "g [of e in USA]"))

  # Make sure it works in a data frame.
  df <- tibble::tibble(labels = c("a [of b in c]", "d [of e in USA]"),
                       new_nouns = c("first_noun", "second_noun"))
  res <- df %>%
    dplyr::mutate(
      with_new_nouns = modify_nouns(labels, new_nouns = new_nouns)
    )
  expect_equal(res$with_new_nouns[[1]], "first_noun [of b in c]")
  expect_equal(res$with_new_nouns[[2]], "second_noun [of e in USA]")

  # Try with different notations for each row
  res2 <- res %>%
    dplyr::mutate(
      # This is a weird way to do things, but it will work.
      notations_col = infer_notation(labels, choose_most_specific = FALSE),
      # When we get inside modify_nouns(), notation is a list,
      # so notations are inferred a second time.
      # But, hey! It works.
      # Probably much better and cleaner to do this the way of res.
      more_new_nouns_from_specific_notations = modify_nouns(labels, new_nouns = new_nouns, notation = notations_col)
    )
  expect_equal(res2$more_new_nouns_from_specific_notations[[1]], "first_noun [of b in c]")
  expect_equal(res2$more_new_nouns_from_specific_notations[[2]], "second_noun [of e in USA]")
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

  # Try with a different label structure without inference
  expect_equal(modify_label_pieces("a -> b", piece = "pref", mod_map = list(new_noun = c("a", "b")),
                                   inf_notation = FALSE,
                                   notation = RCLabels::arrow_notation),
               "new_noun -> b")

  expect_equal(modify_label_pieces(list("a -> b", "c -> d"), piece = "pref", mod_map = list(new_noun = c("a", "b")),
               inf_notation = FALSE,
               notation = RCLabels::arrow_notation),
               list("new_noun -> b", "c -> d"))
})


test_that("modify_label_pieces() works with vectors, lists, and in data frames", {
  # Try with a vector
  labs <- c("a [of b in c]", "d [-> e in f]")
  expect_equal(modify_label_pieces(labs, piece = "noun", mod_map = list(new_noun = c("d", "e"))),
               c("a [of b in c]", "new_noun [-> e in f]"))
  # Try without inference
  expect_equal(modify_label_pieces(labs, piece = "noun", inf_notation = FALSE,
                                   notation = RCLabels::bracket_notation,
                                   mod_map = list(new_noun = c("d", "e"))),
               c("a [of b in c]", "new_noun [-> e in f]"))
  # Try with a list
  labs2 <- list("a [of b in c]", "d [-> e in f]")
  expect_equal(modify_label_pieces(labs2, piece = "->", mod_map = list(`new_->` = c("d", "e"))),
               c("a [of b in c]", "d [-> new_-> in f]"))

  # Try in a data frame
  df <- tibble::tibble(labs = c("a [of b in c]", "d [of e in f]"))
  res <- df %>%
    dplyr::mutate(
      mod1 = modify_label_pieces(labs, piece = "noun", mod_map = list(new_a = c("a", "b", "c")))
    )
  expect_equal(res$mod1, c("new_a [of b in c]", "d [of e in f]"))
})


test_that("modify_label_pieces() works with 2x piece", {
  labs_vector <- c("a [of b in c]", "d [-> e in f]")
  expect_equal(modify_label_pieces(labs_vector,
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
  # Try without inference
  expect_equal(remove_label_pieces(labs,
                                   pieces_to_remove = c("of", "in"),
                                   inf_notation = FALSE,
                                   notation = RCLabels::bracket_notation),
               c("a [ ]", "d [-> e]"))
})
