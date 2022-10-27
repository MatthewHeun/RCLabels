test_that("get_nouns() works as expected", {
  expect_equal(get_nouns("a [b]"), c(noun = "a"))
  expect_equal(get_nouns(c("a [b]", "c [d]")), c(noun = "a", noun = "c"))
  expect_equal(get_nouns(list("a [b]", "c [d]")), c(noun = "a", noun = "c"))
  # Test with wrong notation
  expect_equal(get_nouns(list("a [b]", "c [d]"), notation = RCLabels::arrow_notation),
               c(noun = "a [b]", noun = "c [d]"))

  # Now try in a data frame
  df <- data.frame(labels = c("a [b]", "c [d]", "e [f]", "g [h]"))
  with_nouns <- df %>%
    dplyr::mutate(
      nouns = get_nouns(labels)
    )
  expect_equal(with_nouns$nouns, c(noun = "a", noun = "c", noun = "e", noun = "g"))

  # Try with different notations that are inferred for each label
  expect_equal(get_nouns(c("a -> b", "c [of d]")), c(noun = "a", noun = "c"))
})


test_that("get_nouns() works for a difficult case", {
  # This test was failing for a while.
  expect_equal(get_nouns("a [of b in c]", choose_most_specific = TRUE), c(noun = "a"))
})


test_that("get_nouns() works when inf_notation = FALSE", {
  expect_equal(get_nouns("a [b]",
                         inf_notation = FALSE,
                         notation = RCLabels::bracket_notation),
               c(noun = "a"))

  expect_equal(get_nouns(c("a [b]", "c -> d"),
                         inf_notation = FALSE,
                         notation = list(RCLabels::bracket_notation,
                                         RCLabels::arrow_notation)),
               c(noun = "a", noun = "c"))
})


test_that("get_pps() works as expected", {
  # Try a couple simple ones
  expect_equal(get_pps("a [in b]"), c(pps = "in b"))
  # Not sure why we would ever want to do this.
  expect_equal(get_pps("a [in b]", choose_most_specific = TRUE), c(pps = "b"))
  expect_equal(get_pps("a [in b]", choose_most_specific = FALSE), c(pps = "in b"))
  expect_equal(get_pps(c("a [in b]", "c [of d]"), notation = RCLabels::bracket_notation), c(pps = "in b", pps = "of d"))
  expect_equal(get_pps(c("a [in b]", "c [of d]"), choose_most_specific = FALSE), c(pps = "in b", pps = "of d"))
  # Infer notation
  expect_equal(get_pps(c("a [in b]", "c [of d]"), choose_most_specific = FALSE), c(pps = "in b", pps = "of d"))
  # Try a degenerate case
  expect_equal(get_pps("a [b in c]"), c(pps = "in c"))

  # Try with a single string
  expect_equal(get_pps("a [of b in c]", notation = bracket_notation), c(pps = "of b in c"))

  expect_equal(get_pps(c("a [of b in c]", "d [of e into f]"), notation = bracket_notation),
               c(pps = "of b in c", pps = "of e into f"))

  expect_equal(get_pps(list("a [of b in c]", "d [of e into f]"), notation = bracket_notation),
               c(pps = "of b in c", pps = "of e into f"))

  expect_equal(get_pps("a [in b]", notation = RCLabels::bracket_notation), c(pps = "in b"))
  expect_equal(get_pps(c("a [in b]"), notation = RCLabels::bracket_notation), c(pps = "in b"))
  expect_equal(get_pps(list("a [in b]"), notation = RCLabels::bracket_notation), c(pps = "in b"))

  # Now try in a data frame

  df <- data.frame(labels = c("e [of f in g]", "h [-> i in j]", "a [in b]", "c [of d into USA]"))
  with_nouns_pps <- df %>%
    dplyr::mutate(
      nouns = get_nouns(labels),
      pps = get_pps(labels, choose_most_specific = FALSE)
    )
  expect_equal(with_nouns_pps$nouns, c(noun = "e", noun = "h", noun = "a", noun = "c"))
  expect_equal(with_nouns_pps$pps, c(pps = "of f in g", pps = "-> i in j", pps = "in b", pps = "of d into USA"))
})


test_that("get_pps() works where there are no prepositions", {
  expect_equal(get_pps("a []"), c(pps = ""))
})


test_that("get_pps() works when inf_notation = FALSE", {
  expect_equal(get_pps("a [in b]",
                       choose_most_specific = FALSE),
               c(pps = "in b"))
  expect_equal(get_pps("a [in b]",
                       notation = RCLabels::bracket_notation,
                       choose_most_specific = FALSE),
               c(pps = "in b"))
  expect_equal(get_pps("a [in b]",
                       inf_notation = FALSE,
                       notation = RCLabels::bracket_notation,
                       choose_most_specific = FALSE),
               c(pps = "in b"))
})


test_that("get_prepositions() works correctly", {
  labs <- c("a [-> b in c]", "d [from Production]", "Coal [from Imports into US]")
  # Try with notation inference.
  # This works, because choose_most_specific = FALSE is the default AND
  # bracket_notation is before in_notation, from_notation, and bracket_arrow_notation.
  expect_equal(get_prepositions(labs), list(prepositions = c("->", "in"), prepositions = "from", prepositions = c("from", "into")))
  # Try by specifying bracket_notation
  expect_equal(get_prepositions(labs, notation = RCLabels::bracket_notation),
               list(prepositions = c("->", "in"), prepositions = "from", prepositions = c("from", "into")))
  # Try by turning off notation inference
  expect_equal(get_prepositions(labs, inf_notation = FALSE, notation = RCLabels::bracket_notation),
               list(prepositions = c("->", "in"), prepositions = "from", prepositions = c("from", "into")))
})


test_that("get_objects() works correctly", {
  # Specify the notation being used
  expect_equal(get_objects(c("a [of b in USA]", "d [of e into GBR]"), notation = RCLabels::bracket_notation),
               list(objects = c(of = "b", `in` = "USA"),
                    objects = c(of = "e", into = "GBR")))
  # Specify the notation being used and don't infer.
  expect_equal(get_objects(c("a [of b in USA]", "d [of e into GBR]"),
                           inf_notation = FALSE,
                           notation = RCLabels::bracket_notation),
               list(objects = c(of = "b", `in` = "USA"),
                    objects = c(of = "e", into = "GBR")))
  # Infer notation
  expect_equal(get_objects(c("a [of b in USA]", "d [of e into GBR]")),
               list(objects = c(of = "b", `in` = "USA"),
                    objects = c(of = "e", into = "GBR")))
})


test_that("get_objects() sets preposition with arrow notation", {
  label1 <- "a [-> b]"
  expect_equal(get_objects(label1, notation = RCLabels::bracket_notation, inf_notation = FALSE), list(objects = c(`->` = "b")))
  # This one is weird, but the expected behavior.
  # Note that get_objects() cannot pick up a preposition,
  # because -> is the delimiter between prefix and suffix.
  # The suffix is only "b", with no preposition present.
  label2 <- "a -> b"
  temp <- c(` ` = "b") %>%
    magrittr::set_names(c(""))
  res <- list(objects = temp)
  expect_equal(get_objects(label2), res)
})


test_that("split_noun_pp() works as expected", {
  # Try with specific notation
  expect_equal(split_noun_pp("a [of b]", notation = bracket_notation),
               list(c(noun = "a", of = "b")))
  # Try without inference
  expect_equal(split_noun_pp("a [of b]", inf_notation = FALSE, notation = bracket_notation),
               list(c(noun = "a", of = "b")))
  # Try without notation inference and no notation. This will fail.
  expect_error(split_noun_pp("a [of b]", inf_notation = FALSE))
  # Try with notation inference
  expect_equal(split_noun_pp("a [of b]"),
               list(c(noun = "a", of = "b")))
  expect_equal(split_noun_pp("a [of b in c]"),
               list(c(noun = "a", of = "b", `in` = "c")))
  expect_equal(split_noun_pp(c("a [of b in c]", "d [of e into f]")),
               list(c(noun = "a", of = "b", `in` = "c"),
                    c(noun = "d", of = "e", into = "f")))
  # Try in a list.
  expect_equal(split_noun_pp(list("a [of b in c]",
                                 "d [of e into f]",
                                 "Production [of Coal in GB]")),
               list(c(noun = "a", of = "b", `in` = "c"),
                    c(noun = "d", of = "e", into = "f"),
                    c(noun = "Production", of = "Coal", `in` = "GB")))

  # Now try in a data frame.
  df <- data.frame(labels = I(list("a [in b]", "c [of d into USA]",
                                   "e [of f in g]", "h [-> i in j]")))
  split <- df %>%
    dplyr::mutate(
      splits = split_noun_pp(labels)
    )
  expect_equal(split$splits[[1]], c(noun = "a", `in` = "b"))
  expect_equal(split$splits[[2]], c(noun = "c", of = "d", into = "USA"))
  expect_equal(split$splits[[3]], c(noun = "e", of = "f", `in` = "g"))
  expect_equal(split$splits[[4]], c(noun = "h", `->` = "i", `in` = "j"))
})


test_that("paste_noun_pp() works as expected", {
  # Try with a single label
  lab <- "a [of b in c]"
  split <- split_noun_pp(lab)
  expect_equal(split, list(c(noun = "a", of = "b", `in` = "c")))

  # Try with a vector of labels
  labs <- c("a [of b in c]", "d [from Coal mines in USA]")
  split <- split_noun_pp(labs)
  expect_equal(paste_noun_pp(split), labs)
  # Try with different notations for each paste
  expect_equal(paste_noun_pp(split, notation = list(RCLabels::bracket_notation, RCLabels::arrow_notation)),
               c("a [of b in c]", "d -> from Coal mines in USA"))

  # Try with a weird notation vector
  paren_note <- notation_vec(pref_start = "(", pref_end = ")", suff_start = "(", suff_end = ")")
  labs2 <- c("(Production)(of Coal in USA)", "(Manufacture)(of Oil in Canada)")
  split2 <- split_noun_pp(labs2, notation = paren_note)
  expect_equal(paste_noun_pp(split2, notation = paren_note), labs2)

  # Try in a data frame
  df <- tibble::tibble(labels = c("a [in b]", "c [of d into USA]",
                                  "e [of f in g]", "h [-> i in j]"))
  recombined <- df %>%
    dplyr::mutate(
      splits = split_noun_pp(labels),
      recombined = paste_noun_pp(splits)
    )
  expect_equal(recombined$recombined, recombined$labels)

  # Try with different notations for each row of the data frame.
  df2 <- recombined %>%
    dplyr::mutate(
      notn = list(arrow_notation, in_notation, bracket_notation, bracket_arrow_notation),
      recombined2 = paste_noun_pp(splits, notn)
    )
  expect_equal(df2$recombined2, c("a -> in b", "c [in of d into USA]", "e [of f in g]", "h [-> -> i in j]"))

})


test_that("get_piece() works as expected", {
  labs <- c("a [from b in c]", "d [of e in f]", "Export [of Coal from USA to MEX]")

  # Must have length 1
  expect_error(get_piece(labs, piece = c("from", "to")), "piece must be a character vector of length 1 in RCLabels::get")

  # Returns labs unchanged, because default value for piece is "all".
  expect_equal(get_piece(labs), labs)

  # Prefix
  expect_equal(get_piece(labs, "pref"),
               c(pref = "a", pref = "d", pref = "Export"))
  # Try without inference. This should fail, because no notation is given.
  expect_error(get_piece(labs, piece = "pref", inf_notation = FALSE),
               regexp = "length\\(x\\) and legth\\(notation\\) must be same length in strip_label_part\\(\\)")
  # Default is choose_most_specific = FALSE, which works in this case.
  expect_equal(get_piece(labs, "suff"),
               c(suff = "from b in c", suff = "of e in f", suff = "of Coal from USA to MEX"))
  # With the most specific match, things look weird.
  expect_equal(get_piece(labs, "suff", choose_most_specific = TRUE),
               c(suff = "b in c", suff = "e in f", suff = "Coal from USA to MEX"))
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


test_that("get_piece() does the right thing when it can't infer notation", {
  # It should return the whole label in the prefix when notation can't be inferred.
  expect_equal(split_pref_suff("Crude"), list(pref = "Crude", suff = ""))
  expect_equal(split_pref_suff(c("Crude", "a -> b")),
               list(pref = c("Crude", "a"), suff = c("", "b")))
  expect_equal(split_pref_suff("Crude", inf_notation = FALSE, notation = RCLabels::dash_notation),
               list(pref = "Crude", suff = ""))
  expect_equal(split_pref_suff("Crude", inf_notation = FALSE, notation = RCLabels::arrow_notation),
               list(pref = "Crude", suff = ""))
  expect_equal(get_pref_suff("Crude", which = "pref"), c(pref = "Crude"))
  expect_equal(get_piece("Crude", piece = "noun"), c(noun = "Crude"))
})


test_that("passing NULL to extractors returns NULL", {
  expect_null(get_pref_suff(NULL))
  expect_null(get_nouns(NULL))
  expect_null(get_pps(NULL))
  expect_null(get_prepositions(NULL))
  expect_null(get_objects(NULL))
  expect_null(split_noun_pp(NULL))
  expect_null(paste_noun_pp(NULL))
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


