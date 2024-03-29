% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/notation.R
\name{infer_notation_for_one_label}
\alias{infer_notation_for_one_label}
\title{Infer the notation from one row or column label}
\usage{
infer_notation_for_one_label(
  x,
  inf_notation = TRUE,
  notations = RCLabels::notations_list,
  allow_multiple = FALSE,
  retain_names = FALSE,
  choose_most_specific = TRUE,
  must_succeed = TRUE
)
}
\arguments{
\item{x}{A single row or column label.}

\item{inf_notation}{A boolean that tells whether to infer notation for \code{x}.}

\item{notations}{A list of notations from which matches will be inferred
This function might not work as expected if
\code{notation} is not a list.
If \code{notation} is not a list,
\code{notations} is returned in full.
Default is \code{RCLabels::notations_list}.}

\item{allow_multiple}{A boolean that tells whether multiple notation matches
are allowed.
If \code{FALSE} (the default), multiple matches give an error.}

\item{retain_names}{A boolean that tells whether to retain names on the
outgoing matches.
Default is \code{FALSE}.
If \code{TRUE}, the return value is a named list.
If only one of \code{notations} is returned,
names are never supplied.}

\item{choose_most_specific}{A boolean that indicates if the most-specific notation
will be returned when more than one of \code{notations} matches \code{x}.
Default is \code{TRUE}.}

\item{must_succeed}{A boolean that if \code{TRUE} (the default),
causes an error to be thrown if a matching notation is not found
for any label in \code{x}.
When \code{FALSE}, an unsuccessful label inference will return \code{NULL}.}
}
\value{
A single matching notation object (if \code{allow_multiple = FALSE}, the default)
or possibly multiple matching notation objects (if \code{allow_multiple = TRUE}).
If no \code{notations} match \code{x}, \code{NULL}.
}
\description{
This is a non-public helper function for vectorized \code{infer_notation()}.
}
