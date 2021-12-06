test_that("get_nouns() works as expected", {
  expect_equal(get_nouns("a [b]"), c(noun = "a"))
  expect_equal(get_nouns(c("a [b]", "c [d]")), c(noun = "a", noun = "c"))
  expect_equal(get_nouns(list("a [b]", "c [d]")), c(noun = "a", noun = "c"))

  # Now try in a data frame
  df <- data.frame(labels = c("a [b]", "c [d]", "e [f]", "g [h]"))
  with_nouns <- df |>
    dplyr::mutate(
      nouns = get_nouns(labels)
    )
  expect_equal(with_nouns$nouns, c(noun = "a", noun = "c", noun = "e", noun = "g"))
})


test_that("get_pps() works as expected", {
  # Try a couple simple ones
  expect_equal(get_pps("a [in b]"), "in b")
  expect_equal(get_pps(c("a [in b]", "c [of d]")), c("in b", "of d"))
  # Try a degenerate case
  expect_equal(get_pps("a [b in c]"), "in c")

  # Try with a single string
  expect_equal(get_pps("a [of b in c]", notation = bracket_notation), "of b in c")

  expect_equal(get_pps(c("a [of b in c]", "d [of e into f]"), notation = bracket_notation),
               c("of b in c", "of e into f"))

  expect_equal(get_pps(list("a [of b in c]", "d [of e into f]"), notation = bracket_notation),
               c("of b in c", "of e into f"))

  expect_equal(get_pps("a [in b]"), "in b")
  expect_equal(get_pps(c("a [in b]")), "in b")
  expect_equal(get_pps(list("a [in b]")), "in b")

  # Now try in a data frame

  df <- data.frame(labels = c("e [of f in g]", "h [-> i in j]", "a [in b]", "c [of d into USA]"))
  with_nouns_pps <- df |>
    dplyr::mutate(
      nouns = get_nouns(labels),
      pps = get_pps(labels)
    )
  expect_equal(with_nouns_pps$nouns, c(noun = "e", noun = "h", noun = "a", noun = "c"))
  expect_equal(with_nouns_pps$pps, c("of f in g", "-> i in j", "in b", "of d into USA"))
})


test_that("get_pps() works where there are no prepositions", {
  expect_equal(get_pps("a []"), "")
})


test_that("get_prepositions() works correctly", {
  labs <- c("a [-> b in c]", "d [from Production]", "Coal [from Imports into US]")
  expect_equal(get_prepositions(labs), list(c("->", "in"), "from", c("from", "into")))
})


test_that("get_objects() works correctly", {
  expect_equal(get_objects(c("a [of b in USA]", "d [of e into GBR]")),
               list(c(of = "b", `in` = "USA"),
                    c(of = "e", into = "GBR")))
})


test_that("split_labels() works as expected", {
  expect_equal(split_labels("a [of b]", notation = bracket_notation),
               list(c(noun = "a", of = "b")))
  expect_equal(split_labels("a [of b in c]", notation = bracket_notation),
               list(c(noun = "a", of = "b", `in` = "c")))
  expect_equal(split_labels(c("a [of b in c]", "d [of e into f]"), notation = bracket_notation),
               list(c(noun = "a", of = "b", `in` = "c"),
                    c(noun = "d", of = "e", into = "f")))
  # Try in a list.
  expect_equal(split_labels(list("a [of b in c]",
                                 "d [of e into f]",
                                 "Production [of Coal in GB]"),
                            notation = bracket_notation),
               list(c(noun = "a", of = "b", `in` = "c"),
                    c(noun = "d", of = "e", into = "f"),
                    c(noun = "Production", of = "Coal", `in` = "GB")))

  # Now try in a data frame.
  df <- data.frame(labels = I(list("a [in b]", "c [of d into USA]",
                                   "e [of f in g]", "h [-> i in j]")))
  split <- df |>
    dplyr::mutate(
      splits = split_labels(labels)
    )
  expect_equal(split$splits[[1]], c(noun = "a", `in` = "b"))
  expect_equal(split$splits[[2]], c(noun = "c", of = "d", into = "USA"))
  expect_equal(split$splits[[3]], c(noun = "e", of = "f", `in` = "g"))
  expect_equal(split$splits[[4]], c(noun = "h", `->` = "i", `in` = "j"))
})


test_that("paste_pieces() works as expected", {
  # Try with a single label
  lab <- "a [of b in c]"
  split <- split_labels(lab)
  expect_equal(split, list(c(noun = "a", of = "b", `in` = "c")))

  # Try with a vector of labels
  labs <- c("a [of b in c]", "d [from Coal mines in USA]")
  split <- split_labels(labs)
  expect_equal(paste_pieces(split), labs)

  # Try with a weird notation vector
  paren_note <- notation_vec(pref_start = "(", pref_end = ")", suff_start = "(", suff_end = ")")
  labs2 <- c("(Production)(of Coal in USA)", "(Manufacture)(of Oil in Canada)")
  split2 <- split_labels(labs2, notation = paren_note)
  expect_equal(paste_pieces(split2, notation = paren_note), labs2)

  # Try in a data frame
  df <- tibble::tibble(labels = c("a [in b]", "c [of d into USA]",
                                  "e [of f in g]", "h [-> i in j]"))
  recombined <- df |>
    dplyr::mutate(
      splits = split_labels(labels),
      recombined = paste_pieces(splits)
    )
  expect_equal(recombined$recombined, recombined$labels)
})


