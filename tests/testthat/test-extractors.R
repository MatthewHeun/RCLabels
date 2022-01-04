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
  expect_equal(get_pps("a [in b]"), c(pps = "in b"))
  expect_equal(get_pps(c("a [in b]", "c [of d]")), c(pps = "in b", pps = "of d"))
  # Try a degenerate case
  expect_equal(get_pps("a [b in c]"), c(pps = "in c"))

  # Try with a single string
  expect_equal(get_pps("a [of b in c]", notation = bracket_notation), c(pps = "of b in c"))

  expect_equal(get_pps(c("a [of b in c]", "d [of e into f]"), notation = bracket_notation),
               c(pps = "of b in c", pps = "of e into f"))

  expect_equal(get_pps(list("a [of b in c]", "d [of e into f]"), notation = bracket_notation),
               c(pps = "of b in c", pps = "of e into f"))

  expect_equal(get_pps("a [in b]"), c(pps = "in b"))
  expect_equal(get_pps(c("a [in b]")), c(pps = "in b"))
  expect_equal(get_pps(list("a [in b]")), c(pps = "in b"))

  # Now try in a data frame

  df <- data.frame(labels = c("e [of f in g]", "h [-> i in j]", "a [in b]", "c [of d into USA]"))
  with_nouns_pps <- df |>
    dplyr::mutate(
      nouns = get_nouns(labels),
      pps = get_pps(labels)
    )
  expect_equal(with_nouns_pps$nouns, c(noun = "e", noun = "h", noun = "a", noun = "c"))
  expect_equal(with_nouns_pps$pps, c(pps = "of f in g", pps = "-> i in j", pps = "in b", pps = "of d into USA"))
})


test_that("get_pps() works where there are no prepositions", {
  expect_equal(get_pps("a []"), c(pps = ""))
})


test_that("get_prepositions() works correctly", {
  labs <- c("a [-> b in c]", "d [from Production]", "Coal [from Imports into US]")
  expect_equal(get_prepositions(labs), list(prepositions = c("->", "in"), prepositions = "from", prepositions = c("from", "into")))
})


test_that("get_objects() works correctly", {
  expect_equal(get_objects(c("a [of b in USA]", "d [of e into GBR]")),
               list(objects = c(of = "b", `in` = "USA"),
                    objects = c(of = "e", into = "GBR")))
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


test_that("get_piece() works as expected", {
  labs <- c("a [from b in c]", "d [of e in f]", "Export [of Coal from USA to MEX]")

  # Must have length 1
  expect_error(get_piece(labs, piece = c("from", "to")), "piece must be a character vector of length 1 in RCLabels::get")

  # Returns labs unchanged, because default value for piece is "all".
  expect_equal(get_piece(labs), labs)

  # Prefix and suffix
  expect_equal(get_piece(labs, "pref"),
               c(pref = "a", pref = "d", pref = "Export"))
  expect_equal(get_piece(labs, "suff"),
               c(suff = "from b in c", suff = "of e in f", suff = "of Coal from USA to MEX"))
  # Noun
  expect_equal(get_piece(labs, piece = "noun"),
               c(noun = "a", noun = "d", noun = "Export"))
  # Prepositions
  expect_equal(get_piece(labs, "from"),
               list(c(from = "b"), c(from = ""), c(from = "USA")))
  expect_equal(get_piece(labs, "in"),
               list(c(`in` = "c"), c(`in` = "f"), c(`in` = "")))
  expect_equal(get_piece(labs, "of"),
               list(c(of = ""), c(of = "e"), c(of = "Coal")))
  expect_equal(get_piece(labs, "from"),
               list(c(from = "b"), c(from = ""), c(from = "USA")))
  # Unknown preposition
  expect_equal(get_piece(labs, "bogus"),
               list(c(bogus = ""), c(bogus = ""), c(bogus = "")))
})


test_that("get_piece() works with 'pps'", {
  labs <- c("a [from b in c]", "d [of e in f]", "Export [of Coal from USA to MEX]")
  expect_equal(get_piece(labs, piece = "pps"),
               c(pps = "from b in c", pps = "of e in f", pps = "of Coal from USA to MEX"))
})


test_that("get_piece() works with 'prepositions'", {
  labs <- c("a [from b in c]", "d [of e in f]", "Export [of Coal from USA to MEX]")
  expect_equal(get_piece(labs, piece = "prepositions"),
               list(prepositions = c("from", "in"), prepositions = c("of", "in"), prepositions = c("of", "from", "to")))
})


test_that("get_piece() works with 'objects'", {
  labs <- c("a [from b in c]", "d [of e in f]", "Export [of Coal from USA to MEX]")
  expect_equal(get_piece(labs, piece = "objects"),
               list(objects = c(from = "b", `in` = "c"),
                    objects = c(of = "e", `in` = "f"),
                    objects = c(of = "Coal", from = "USA", to = "MEX")))

})


test_that("passing NULL to extractors returns NULL", {
  expect_null(get_pref_suff(NULL))
  expect_null(get_nouns(NULL))
  expect_null(get_pps(NULL))
  expect_null(get_prepositions(NULL))
  expect_null(get_objects(NULL))
  expect_null(split_labels(NULL))
  expect_null(paste_pieces(NULL))
})


test_that("get_piece(NULL) returns NULL", {
  labs <- c("a [from b in c]", "d [of e in f]", "Export [of Coal from USA to MEX]")
  expect_null(get_piece(NULL, piece = "pref"))
  expect_null(get_piece(NULL, piece = "suff"))
  expect_null(get_piece(NULL, piece = "noun"))
  expect_null(get_piece(NULL, piece = "pps"))
  expect_null(get_piece(NULL, piece = "prepositions"))
  expect_null(get_piece(NULL, piece = "objects"))
})


