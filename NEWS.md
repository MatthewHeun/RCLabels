---
title: "Release notes for `RCLabels`"
output: html_document
---


* `RCLabels::prepositions` is now a vector instead of a list,
  thereby making downstream use of the object clearer.
* Breaking changes
    - `keep_pref_suff()` --> `get_pref_suff()` to bring consistency with `get_piece()`.
    - `keep` --> `which` change name of argument in `get_pref_suff()`.
* New wrapper function `get()` returns requested piece of a label.
* Added note to README.Rmd about installing from CRAN.
* Added project status badge.
* Added CRAN status badge.


# RCLabels 0.0.4 (2021-12-06)

* New function `replace_by_pattern()`.
* New function `match_by_pattern()`.
* First CRAN release.
* New tests for new functions.
    * 187 tests, all passing.
    * Test coverage remains at 100 %.


# RCLabels 0.0.3

* Added code coverage.
* Added automated spell checking to the package.
* No new tests.
    * 156 tests, all passing.
    * Test coverage remains at 100 %.


# RCLabels 0.0.2

* First release.
* Added GitHub pages site.
* Added a vignette.
* Added extraction functions. 
* Added a `NEWS.md` file to track changes to the package.
* Refactoring many functions out of IEATools.
* Many tests for all features.
    * 156 tests, all passing.
    * Test coverage is at 100 %.


# RCLabels 0.0.1

* First commit.
