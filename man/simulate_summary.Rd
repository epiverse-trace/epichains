% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate.r
\name{simulate_summary}
\alias{simulate_summary}
\title{Simulate a vector of transmission chains sizes/lengths}
\usage{
simulate_summary(
  index_cases,
  statistic = c("size", "length"),
  offspring_dist,
  ...,
  stat_max = Inf,
  pop = Inf,
  percent_immune = 0
)
}
\arguments{
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

\item{...}{Parameters of the offspring distribution as required by R.}

\item{stat_max}{A cut off for the chain statistic (size/length) being
computed. A number coercible to integer. Results above the specified value,
are set to \code{Inf}.}

\item{pop}{Population size; An \verb{<Integer>}. Used alongside \code{percent_immune}
to define the susceptible population. Defaults to \code{Inf}.}

\item{percent_immune}{Percent of the population immune to
infection at the start of the simulation; A \verb{<numeric>} between 0 and 1.
Used alongside \code{pop} to initialise the susceptible population. Defaults to
0.}
}
\value{
An object of class \verb{<epichains_summary>}, which is a numeric
vector of chain sizes or lengths with extra attributes for storing the
simulation parameters.
}
\description{
Generates a vector of transmission chain sizes and lengths with a
value for each index case. It uses a simple branching process model that
accepts an index number of cases that seed the outbreak, a distribution
of offspring per case, and a chain size or length/duration (statistic)
to track. It optionally accepts other population related inputs
such as the population size (defaults to Inf) and percentage of the
population initially immune (defaults to 0).
}
\section{\code{simulate_summary()} vs \code{simulate_chains()}}{
\code{simulate_summary()} is a time-invariant version of \code{simulate_chains()}. In
particular, it does not track the details of individual transmission events
but deals with eventual chain statistics, that is, the statistic
realised by a chain after dying out.

It is useful for generating a vector of chain sizes or lengths for a given
number of index cases, if details of who infected whom and the timing of
infection are not of interest.

This function is used in \code{{epichains}} for calculating likelihoods in
the \code{likelihood()} function and for sampling from the borel
distribution (See ?epichains::rborel). It is used extensively in the
vignette on
\href{https://epiverse-trace.github.io/epichains/articles/interventions.html}{modelling disease control},
where only data on observed chain sizes and lengths are available.
}

\section{Calculating chain sizes and lengths}{
The function simulates the chain size for individual \eqn{i} at time
\eqn{t}, \eqn{I_{t, i}}, as:
\deqn{I_{t, i} = \sum_{i}^{I_{t-1}}X_{t, i},}
and the chain length/duration for individual \eqn{i} at time \eqn{t},
\eqn{L_{t, i}}, as:
\deqn{L_{t, i} = {\sf min}(1, X_{t, i}), }
where \eqn{X_{t, i}} is the secondary cases generated by individual \eqn{i}
at time \eqn{t}, and \eqn{I_{0, i} = L_{0, i} = 1}.

The distribution of secondary cases, \eqn{X_{t, i}} is modelled by the
offspring distribution (\code{offspring_dist}).
}

\examples{
# Simulate chain sizes with a poisson offspring distribution, assuming an
# infinite population and no immunity.
set.seed(32)
simulate_summary(
  index_cases = 20,
  statistic = "size",
  offspring_dist = rpois,
  stat_max = 10,
  lambda = 0.9
)
# Simulate chain sizes with a negative binomial distribution and assuming
# a finite population and 10\% immunity.
set.seed(32)
simulate_summary(
  pop = 1000,
  percent_immune = 0.1,
  index_cases = 20,
  statistic = "size",
  offspring_dist = rnbinom,
  stat_max = 10,
  mu = 0.9,
  size = 0.36
)
}
\references{
Jacob C. (2010). Branching processes: their role in epidemiology.
International journal of environmental research and public health, 7(3),
1186–1204. \doi{https://doi.org/10.3390/ijerph7031204}

Blumberg, S., and J. O. Lloyd-Smith. 2013. "Comparing Methods for
Estimating R0 from the Size Distribution of Subcritical Transmission
Chains." Epidemics 5 (3): 131–45.
\doi{https://doi.org/10.1016/j.epidem.2013.05.002}.

Farrington, C. P., M. N. Kanaan, and N. J. Gay. 2003.
"Branching Process Models for Surveillance of Infectious Diseases
Controlled by Mass Vaccination.” Biostatistics (Oxford, England)
4 (2): 279–95. \doi{https://doi.org/10.1093/biostatistics/4.2.279}.
}
\author{
James M. Azam, Sebastian Funk
}
