% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_isoplotr.R
\name{tidy_isoplotr}
\alias{tidy_isoplotr}
\title{Calculate U-Pb Ages in a Tidy Format}
\usage{
tidy_isoplotr(df, age_type = 1)
}
\arguments{
\item{df}{A data frame with columns for isotopic ratios and uncertainties required by \code{use_isoplotr}.}

\item{age_type}{Numeric specifying the type of age to compute in \code{use_isoplotr}. Default is 1.}
}
\value{
A data frame with U-Pb ages and uncertainties for each row in \code{df}.
}
\description{
This function applies \code{use_isoplotr} to each row of a data frame, calculating U-Pb ages for each sample
and returning a combined result in a tidy format.
}
\examples{
# Calculate U-Pb ages in a tidy format for each row of `df`
tidy_isoplotr(df, age_type = 1)
}
