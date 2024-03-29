% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chk.R
\name{chk-values}
\alias{chk-values}
\alias{chk_equals}
\alias{chk_values}
\alias{chk_range}
\alias{chk_blank}
\title{Checks: values}
\usage{
chk_equals(x, val)

chk_values(x, ..., miss = getOption("testdat.miss"))

chk_range(x, min, max, ...)

chk_blank(x)
}
\arguments{
\item{x}{A vector to check.}

\item{val}{A scalar value for the equality check.}

\item{...}{Vectors of valid values.}

\item{miss}{A vector of values to be treated as missing. The
\link[=testdat]{testdat.miss} or \link[=testdat]{testdat.miss_text} option is used by
default.}

\item{min}{Minimum value for range check.}

\item{max}{Maximum value for range check.}
}
\value{
A logical vector flagging records that have passed or failed the
check.
}
\description{
Check that a vector contains only certain values.
}
\examples{

x <- c(NA, 0, 1, 0.5, 0, NA, 99)
chk_blank(x) # Blank
chk_equals(x, 0) # Either blank or 0
chk_values(x, 0, 1) # Either blank, 0, 1, or 99
chk_range(x, 0, 1) # Either blank or in [0,1]
chk_range(x, 0, 1, 99) # Either blank, in [0,1], or equal to 99

}
\seealso{
\link[=chk-helper]{Checks: data frame helpers}

\link[=value-expectations]{Expectations: values}

Other vector checks: 
\code{\link{chk-dates}},
\code{\link{chk-dummy}},
\code{\link{chk-labels}},
\code{\link{chk-patterns}},
\code{\link{chk-text}},
\code{\link{chk-uniqueness}}
}
\concept{vector checks}
