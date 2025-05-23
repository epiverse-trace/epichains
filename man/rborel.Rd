% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/borel.R
\name{rborel}
\alias{rborel}
\title{Generate random numbers from the Borel distribution}
\usage{
rborel(n, mu, censor_at = Inf)
}
\arguments{
\item{n}{Number of random variates to generate.}

\item{mu}{A non-negative number for the poisson mean.}

\item{censor_at}{A stopping criterion; \verb{<numeric>}. Defaults to \code{Inf}. A
value above which the simulation ends and the random number is set to
\code{Inf} (as a form of censoring). \code{rborel()} simulates chain sizes using
\code{\link[=simulate_chain_stats]{simulate_chain_stats()}} with a Poisson offspring distribution, so if
\code{mu >= 1}, the simulation could proceed unendingly. This parameter is used
to prevent this.}
}
\value{
A numeric vector of random numbers.
}
\description{
Random numbers are generated by simulating from a Poisson branching process
}
\examples{
set.seed(32)
rborel(5, 1)
}
\author{
Sebastian Funk James M. Azam
}
