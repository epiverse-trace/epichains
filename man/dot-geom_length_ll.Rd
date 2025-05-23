% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_likelihoods.R
\name{.geom_length_ll}
\alias{.geom_length_ll}
\title{Log-likelihood of the length of chains with geometric offspring distribution}
\usage{
.geom_length_ll(x, prob)
}
\arguments{
\item{x}{A numeric vector of chain lengths.}

\item{prob}{The probability of the geometric distribution with mean
\code{1/prob}. A single numeric value between 0 and 1.}
}
\value{
A numeric vector of log-likelihood values.
}
\description{
Log-likelihood of the length of chains with geometric offspring distribution
}
\author{
Sebastian Funk James M. Azam
}
\keyword{internal}
