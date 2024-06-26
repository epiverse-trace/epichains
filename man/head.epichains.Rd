% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epichains.R
\name{head.epichains}
\alias{head.epichains}
\alias{tail.epichains}
\title{\code{head} and \code{tail} method for \verb{<epichains>} class}
\usage{
\method{head}{epichains}(x, ...)

\method{tail}{epichains}(x, ...)
}
\arguments{
\item{x}{An \verb{<epichains>} object.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
An object of class \verb{<data.frame>}.
}
\description{
\code{head} and \code{tail} method for \verb{<epichains>} class
}
\details{
This returns the top rows of an \verb{<epichains>} object, starting from the
first known infectors.

To view the full output, use \verb{as.data.frame(<object_name>)}.
}
\examples{
set.seed(32)
chains_pois_offspring <- simulate_chains(
  n_chains = 10,
  statistic = "size",
  offspring_dist = rpois,
  stat_threshold = 10,
  generation_time = function(n) rep(3, n),
  lambda = 2
)
head(chains_pois_offspring)
set.seed(32)
chains_pois_offspring <- simulate_chains(
  n_chains = 10,
  statistic = "size",
  offspring_dist = rpois,
  stat_threshold = 10,
  generation_time = function(n) rep(3, n),
  lambda = 2
)
tail(chains_pois_offspring)
}
\author{
James M. Azam
}
