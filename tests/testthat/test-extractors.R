test_that("get_noun() works as expected", {
  expect_equal(get_nouns("a [b]"), list("a"))
  expect_equal(get_nouns(c("a [b]", "c [d]")), list("a", "c"))
  expect_equal(get_nouns(list("a [b]", "c [d]")), list("a", "c"))

  # Now try in a data frame
  df <- data.frame(labels = I(list(list("a [b]", "c [d]"),
                                   list("e [f]", "g [h]"))))
  with_nouns <- df |>
    dplyr::mutate(
      nouns = get_nouns(labels)
    )
  expect_equal(with_nouns$nouns[[1]], c("a", "c"))
  expect_equal(with_nouns$nouns[[2]], c("e", "g"))
})



test_that("get_pps() works as expected", {
  expect_equal(get_pps(c("a [of b in c]", "d [of e into f]"), bracket_notation),
               list(list("of b", "in c"), list("of e", "into f")))
  expect_equal(get_pps(list("a [of b in c]", "d [of e into f]"), bracket_notation),
               list(list("of b", "in c"), list("of e", "into f")))

  expect_equal(get_pps("a [in b]"), list(list("in b")))
  expect_equal(get_pps(c("a [in b]")), list(list("in b")))
  expect_equal(get_pps(list("a [in b]")), list(list("in b")))

  # Now try in a data frame

  df <- data.frame(labels = I(list(list("e [of f in g]", "h [-> i in j]"),
                                   list("a [in b]", "c [of d]"))))
  with_nouns_pps <- df |>
    dplyr::mutate(
      nouns = get_nouns(labels),
      pps = get_pps(labels)
    )
  expect_equal(with_nouns_pps$nouns[[1]], c("e", "h"))
  expect_equal(with_nouns_pps$nouns[[2]], c("a", "c"))


})
