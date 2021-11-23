test_that("get_noun() works as expected", {
  expect_equal(get_nouns("a [b]"), "a")
  expect_equal(get_nouns(c("a [b]", "c [d]")), list("a", "c"))
  expect_equal(get_nouns(list("a [b]", "c [d]")), list("a", "c"))

  # Now try in a data frame
  df <- data.frame(labels = I(list(list("a [b]", "c [d]"),
                                   list("e [f]", "g [h]"))))
  with_nouns <- df |>
    dplyr::mutate(
      nouns = get_nouns(labels)
    )
  expect_equal(with_nouns$nouns[[1]], list("a", "c"))
  expect_equal(with_nouns$nouns[[2]], list("e", "g"))
})


test_that("get_pps() works as expected", {
  # Try with a single string
  expect_equal(get_pps("a [of b in c]", notation = bracket_notation), c("of b", "in c"))

  expect_equal(get_pps(c("a [of b in c]", "d [of e into f]"), notation = bracket_notation),
               list(c("of b", "in c"), c("of e", "into f")))




  expect_equal(get_pps(list("a [of b in c]", "d [of e into f]"), notation = bracket_notation),
               list(c("of b", "in c"), c("of e", "into f")))

  expect_equal(get_pps("a [in b]"), "in b")
  expect_equal(get_pps(c("a [in b]")), "in b")
  expect_equal(get_pps(list("a [in b]")), "in b")

  # Now try in a data frame

  df <- data.frame(labels = I(list(list("e [of f in g]", "h [-> i in j]"),
                                   list("a [in b]", "c [of d into USA]"))))
  with_nouns_pps <- df |>
    dplyr::mutate(
      nouns = get_nouns(labels),
      pps = get_pps(labels)
    )
  expect_equal(with_nouns_pps$nouns[[1]], list("e", "h"))
  expect_equal(with_nouns_pps$nouns[[2]], list("a", "c"))

  expect_equal(with_nouns_pps$pps[[1]], list(c("of f", "in g"), c("-> i", "in j")))
  expect_equal(with_nouns_pps$pps[[2]], list("in b", c("of d", "into USA")))
})


test_that("split_labels() works as expected", {
  expect_equal(split_labels("a [of b]", notation = bracket_notation),
               list(noun = "a", pps = "of b"))
  expect_equal(split_labels("a [of b in c]", notation = bracket_notation),
               list(noun = "a", pps = c("of b", "in c")))
  expect_equal(split_labels(c("a [of b in c]", "d [of e into f]"), notation = bracket_notation),
               list(list(noun = "a", pps = c("of b", "in c")),
                    list(noun = "d", pps = c("of e", "into f"))))
  # Try in a list.
  # This would be like a column in a data frame.
  expect_equal(
    split_labels(list(c("a [of b]", "c [of d]"),
                      c("e [-> f]", "g [of h in i]")), notation = bracket_notation),
    list(list(list(noun = "a", pps = "of b")),
              list(noun = "c", pps = "of d")),
         list(list(noun = "e", pps = "-> f"),
              list(noun = "g", pps = c("of h", "in i"))))




  # Now try in a data frame.
  # df <- data.frame(labels = I(list(list("a [in b]", "c [of d into USA]"),
  #                                  list("e [of f in g]", "h [-> i in j]"))))
  # splitted <- df |>
  #   dplyr::mutate(
  #     splits = split_labels(labels)
  #   )
})
