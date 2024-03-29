% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chk.R
\name{chk-uniqueness}
\alias{chk-uniqueness}
\alias{chk_unique}
\title{Checks: uniqueness}
\usage{
chk_unique(x)
}
\arguments{
\item{x}{A vector to check.}
}
\value{
A logical vector flagging records that have passed or failed the
check.
}
\description{
Check that each value in a vector is unique.
}
\examples{

x <- c(NA, 1:10, NA)
chk_unique(x)

x <- c(10, 1:10, 10)
chk_unique(x)

}
\seealso{
\link[=chk-helper]{Checks: data frame helpers}

\link[=uniqueness-expectations]{Expectations: uniqueness}

Other vector checks: 
\code{\link{chk-dates}},
\code{\link{chk-dummy}},
\code{\link{chk-labels}},
\code{\link{chk-patterns}},
\code{\link{chk-text}},
\code{\link{chk-values}}
}
\concept{vector checks}
