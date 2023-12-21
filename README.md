
<!-- README.md is generated from README.Rmd. Please edit README.Rmd. -->

# RCLabels

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/RCLabels)](https://CRAN.R-project.org/package=RCLabels)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test
coverage](https://codecov.io/gh/MatthewHeun/RCLabels/branch/main/graph/badge.svg)](https://app.codecov.io/gh/MatthewHeun/RCLabels?branch=main)
[![R-CMD-check](https://github.com/MatthewHeun/RCLabels/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MatthewHeun/RCLabels/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5819143.svg)](https://doi.org/10.5281/zenodo.5819143)
<!-- badges: end -->

## Statement of need

When analyzing lists of matrices (whether actual `list`s or matrix
columns in a [matsindf](https://github.com/MatthewHeun/matsindf) data
frame), it can be challenging to operate on matrix row and column names.
This package provides functions to assist with manipulating matrix row
and column labels for those situations. Example applications include
economic and energy conversion chain analyses using input-output (IO)
analysis or physical supply-use table (PSUT) analysis.

## Installation

You can install `RCLabels` from CRAN with:

``` r
install.packages("RCLabels")
```

You can install the development version of `RCLabels` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MatthewHeun/RCLabels")
```

## More Information

Find more information, including vignettes and function documentation,
at <https://MatthewHeun.github.io/RCLabels/>.
