---
title: "Release notes for `RCLabels`" 
output: html_document
---

Cite all releases with doi [10.5281/zenodo.5819143](https://doi.org/10.5281/zenodo.5819143), 
which always resolves to the latest release.


## RCLabels 0.1.11 (2024-01-26)

* `match_by_pattern()` gains new arguments such as
  `inf_notation` and `choose_most_specific`
  which are passed to `split_noun_pp()`.
  Crucially, these new arguments prevent passing via `...`
  to `grepl()` causing an error in an upcoming release
  of `matsbyname`.
* Added clearer documentation to `get_prepositions()`.
* Added new tests that illustrate correct results,
  even if they are unexpected behaviors.
  In short, usually need to use `bracket_notation` 
  (instead of `from_notation` or similar)
  when calling `get_prepositions()`.


## RCLabels 0.1.10 (2024-01-26) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10583900.svg)](https://doi.org/10.5281/zenodo.10583900)

* Tests now run in parallel.
* Now a better test for inferring notation when the `notation` argument
  is not a list.
* Better documentation of the behavior of `infer_notation()`.
* New tests for existing features.
    * Now up to 374 tests, all passing.
    * Test coverage remains at 100%.


## RCLabels 0.1.9 (2023-12-20) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10418779.svg)](https://doi.org/10.5281/zenodo.10418779)

* Added a statement of need to the `README.Rmd` file.
* No new tests.
    * Still at 372 tests, all passing.
    * Test coverage remains at 100%.


## RCLabels 0.1.8 (2023-12-09) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10340402.svg)](https://doi.org/10.5281/zenodo.10340402)

* Fixing GitHub pkgdown website.
* No new tests.
    * Still at 372 tests, all passing.
    * Test coverage remains at 100%.


## RCLabels 0.1.7 (2023-11-30) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10247245.svg)](https://doi.org/10.5281/zenodo.10247245)

* Added code of conduct and contributing pages to documentation.
* Fixed typos in README.Rmd
* No new tests.
    * Still at 372 tests, all passing.
    * Test coverage remains at 100%.


## RCLabels 0.1.6 (2023-07-28) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8193819.svg)](https://doi.org/10.5281/zenodo.8193819)

* This version not released to CRAN.
* Changed the citation URL to point to Zenodo.
* No new tests
    * Still at 372 tests, all passing.
    * Test coverage remains at 100%.


## RCLabels 0.1.5 (2023-05-04) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7896903.svg)](https://doi.org/10.5281/zenodo.7896903)

* Move to latest version of GitHub test coverage workflow.
* This version not released to CRAN.
* No new tests
    * Still at 372 tests, all passing.
    * Test coverage remains at 100%.


## RCLabels 0.1.4 (2023-04-25) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7863791.svg)](https://doi.org/10.5281/zenodo.7863791)

* Fixed a bug where a `list()` was returned when
  `character()` should have been returned
  for zero-length inputs.
* A few new tests
    * Now at 372 tests, all passing
    * Test coverage remains at 100%.


## RCLabels 0.1.3 (2023-01-16) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7541413.svg)](https://doi.org/10.5281/zenodo.7541413)

* This version was not submitted to CRAN.
  It is purely a backend maintenance release.
* Updated the version of github actions to latest from the `usethis` package
  in an attempt to fix issues with the actions failing on github.
* No new capabilities.
    * Still at 368 tests, all passing.
    * Test coverage remains at 100%.


## RCLabels 0.1.2 (2022-10-28) [![DOI](https://zenodo.org/badge/429532436.svg)](https://zenodo.org/badge/latestdoi/429532436)

* A failing test led to new capability:
  `modify_label_pieces()` now allows 
  `piece = "pref"` and `piece = "suff"`.
* Accepted a merge request from Hadley Wickham.
* Submitting `character()` (an empty character vector)
  to `paste_pref_suff()` now causes an error.
  Use `""` instead.
* Fixed a bug where named values in `pref` and `suff`
  caused `paste_pref_suff()` to fail. 
* When notation elements are present in multiple
  locations in a row or column label,
  an error is no longer thrown. 
  The first location is now reported.
* When notation cannot be inferred, 
  `get_piece()` and similar functions
  now return the full label in the prefix. 
  (Previously this had been an error.)
  This new behavior is similar to 
  returning the entire label in the prefix
  when notation is supplied (not inferred) and 
  the notation is not applicable for the label.
* Added `dash_notation`.
* `paste_pref_suff()` now recycles arguments of length 1.
* All functions that paste row and column label pieces together
  are now vectorized over `notation` arguments.
* All functions that get pieces of row and column labels
  now `infer_notation()` by default, 
  because new argument `inf_notation` is set to `TRUE`.
* Rename `paste_pieces()` --> `paste_noun_pp()`.
* Rename `split_labels()` --> `split_noun_pp()`.
* Rename `RCLabels::prepositions` --> `RCLabels::prepositions_list`
  to bring consistency with `RCLables::notations_list`.
* New function `infer_notation()` obtains the notation 
  for any label or a vector of labels.
* Now includes a list of known notations: `RCLabels::notations_list`.
* Added `RCLabels::in_notation`.
* Fixed `to_notation` example in the documentation.
  (The example for `to_notation` was `from_notation`.)
* Improved accuracy of documentation for `get_pps()`.
* New tests for new capabilities.
    * Now up to 368 tests, all passing.
    * Test coverage remains at 100%.


## RCLabels 0.1.1 (2022-03-05) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6331050.svg)](https://doi.org/10.5281/zenodo.6331050)

* Added backward compatibility with previous versions of R
  via reverting to the magrittr pipe (`%>%`) from the system pipe (`|>`).
* Added new notation type `first_dot_notation`.
* New tests for new capabilities.
    * Now up to 225 tests, all passing.
    * Test coverage remains at 100%.


## RCLabels 0.1.0 (2022-01-03) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5819144.svg)](https://doi.org/10.5281/zenodo.5819144)

* First version to be used by other packages, so bumping to v0.1.0.
* `get_piece(labels = NULL)` now returns `NULL`, as expected.
* All return values from `get_piece()` are now named appropriately.
* Added new options for the `piece` argument of `get_piece()`: "pps", 
  "prepositions", and "objects".
* `RCLabels::prepositions` is now a vector instead of a list,
  thereby making downstream use of the object clearer.
* Breaking changes
    - `keep_pref_suff()` --> `get_pref_suff()` to bring consistency with `get_piece()`.
    - `keep` --> `which` for argument name in `get_pref_suff()`
      to bring consistency with other functions.
* New wrapper function `get_piece()` returns requested piece of a label.
* Added note to README.Rmd about installing from CRAN.
* Added project status badge.
* Added CRAN status badge.
* New tests for new functions.
    * Now up to 216 tests, all passing.
    * Test coverage remains at 100%.


## RCLabels 0.0.4 (2021-12-06)

* New function `replace_by_pattern()`.
* New function `match_by_pattern()`.
* First CRAN release.
* New tests for new functions.
    * 187 tests, all passing.
    * Test coverage remains at 100 %.


## RCLabels 0.0.3

* Added code coverage.
* Added automated spell checking to the package.
* No new tests.
    * 156 tests, all passing.
    * Test coverage remains at 100 %.


## RCLabels 0.0.2

* First release.
* Added GitHub pages site.
* Added a vignette.
* Added extraction functions. 
* Added a `NEWS.md` file to track changes to the package.
* Refactoring many functions out of IEATools.
* Many tests for all features.
    * 156 tests, all passing.
    * Test coverage is at 100 %.


## RCLabels 0.0.1

* First commit.
