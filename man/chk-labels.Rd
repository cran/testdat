% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expect-labels.R
\name{chk-labels}
\alias{chk-labels}
\alias{chk_labels}
\title{Checks: labels}
\usage{
chk_labels(x, val_labels = NULL, var_label = NULL)
}
\arguments{
\item{x}{A vector to check.}

\item{val_labels}{What value label check should be performed? One of:
\itemize{
\item A character vector of expected value labels.
\item A named vector of expected label-value pairs.
\item \code{TRUE} to test for the presence of value labels in general.
\item \code{FALSE} to test for the absence of value labels.
\item \code{NULL} to ignore value labels when checking.
}}

\item{var_label}{What variable label check should be performed? One of:
\itemize{
\item A character vector of expected variable labels.
\item \code{TRUE} to test for the presence of a variable labels.
\item \code{FALSE} to test for the absence of a variable labels.
\item \code{NULL} to ignore the variable label when checking.
}}
}
\value{
A logical vector flagging records that have passed or failed the
check.
}
\description{
Check that a vector is labelled in a given way.
}
\examples{

df <- data.frame(
  x = labelled::labelled(c("M", "M", "F"), c(Male = "M", Female = "F"), "Sex"),
  y = labelled::labelled(c("M", "M", "F"), c(Male = "M", Female = "F", Other = "X")),
  z = c("M", "M", "F")
)

# Check for a value-label pairing
chk_labels(df$x, c(Male = "M"))

# Check that two variables have the same values
chk_labels(df$x, labelled::val_labels(df$y))

# Check for the presence of a particular label
chk_labels(df$x, "Male")
chk_labels(df$x, var_label = "Sex")

# Check that a variable is labelled at all
chk_labels(df$z, val_labels = TRUE)
chk_labels(df$z, var_label = TRUE)

# Check that a variable isn't labelled
chk_labels(df$z, val_labels = FALSE)
chk_labels(df$z, var_label = FALSE)

}
\seealso{
\link[=chk-helper]{Checks: data frame helpers}

\link[=label-expectations]{Expectations: labels}

Other vector checks: 
\code{\link{chk-dates}},
\code{\link{chk-dummy}},
\code{\link{chk-patterns}},
\code{\link{chk-text}},
\code{\link{chk-uniqueness}},
\code{\link{chk-values}}
}
\concept{vector checks}
