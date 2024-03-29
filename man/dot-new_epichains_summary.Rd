% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epichains.R
\name{.new_epichains_summary}
\alias{.new_epichains_summary}
\title{Construct a \verb{<epichains_summary>} object}
\usage{
.new_epichains_summary(
  chains_summary,
  index_cases,
  statistic,
  offspring_dist,
  stat_max
)
}
\arguments{
\item{chains_summary}{A numeric \verb{<vector>} of chain sizes and lengths.}

\item{index_cases}{Number of index cases to simulate transmission chains for.}

\item{statistic}{The chain statistic to track as the
stopping criteria for each chain being simulated when \code{stat_max} is not
\code{Inf}; A \verb{<string>}. It can be one of:
\itemize{
\item "size": the total number of cases produced by a chain before it goes
extinct.
\item "length": the total number of ancestors produced by a chain before
it goes extinct.
}}

\item{offspring_dist}{Offspring distribution: a \verb{<function>} like the ones
provided by R to generate random numbers from given distributions (e.g.,
\code{\link{rpois}} for Poisson). More specifically, the function needs to
accept at least one argument, \code{n}, which is the number of random
numbers to generate. It can accept further arguments, which will be passed
on to the random number generating functions. Examples that can be provided
here are \code{rpois} for Poisson distributed offspring, \code{rnbinom} for negative
binomial offspring, or custom functions.}

\item{stat_max}{The cut off for the chain statistic (size/length) being
computed; A number coercible to integer. Results above \code{stat_max} are set to
\code{stat_max}; An \verb{<integer>}. Defaults to \code{Inf}.}
}
\description{
\code{new_epichains_summary()} constructs an \verb{<epichains_summary>} object from a
supplied \verb{<vector>} of chain sizes or lengths. It also stores extra
attributes passed as individual arguments.

\code{new_epichains_summary()} is meant to be lazy and performant, by creating
the object without checking the arguments for correctness. It is not safe
to call \code{new_epichains_summary()} on its own as is called within
\code{epichains_summary()} after the arguments have been checked. To create a
new \verb{<epichains_summary>} object safely, use \code{epichains_summary()}.
}
\author{
James M. Azam
}
\keyword{internal}
