% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epichains.R
\name{summary.epichains_summary}
\alias{summary.epichains_summary}
\title{Summary method for \verb{<epichains_summary>} class}
\usage{
\method{summary}{epichains_summary}(object, ...)
}
\arguments{
\item{object}{An \verb{<epichains_summary>} object.}

\item{...}{Not used.}
}
\value{
A list of chain summaries. The list contains the following
elements:
\itemize{
\item \code{n_chains}: the number of chains simulated.
\item \code{max_stat}: the maximum chain statistic (size/length) achieved by the
chains.
\item \code{min_stat}: the minimum chain statistic (size/length) achieved by the
chains.
}
}
\description{
Summary method for \verb{<epichains_summary>} class
}
\examples{
# Using a Poisson offspring distribution and simulating from an infinite
# population up to chain size 10.
set.seed(32)
chain_stats <- simulate_chain_stats(
  n_chains = 10,
  statistic = "size",
  offspring_dist = rpois,
  stat_threshold = 10,
  lambda = 2
)
summary(chain_stats)
}
\author{
James M. Azam
}
