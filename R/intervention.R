#' Set up intervention for simulation
#'
#' @description
#' `intvn_scale_r0()` is a helper for the \code{simulation_*} functions. It
#' modifies the relevant arguments of the offspring distribution in order to
#' mimic the impact of an intervention. In particular, it scales the mean of
#' the offspring distribution. Currently, it can only handle the poisson and
#' negative binomial distributions and errors when other offspring
#' distributions are specified alongside `intvn_scale_r0`.
#'
#' @inheritParams simulate_tree
#' @param r0_reduction The intervention impact. A scalar between 0 and 1.
#' Scales the mean of `offspring_dist`. `r0_reduction` = 0 implies
#' no intervention impact and `r0_reduction` = 1 implies full impact.
#' @param pars_list Parameter(s) for poisson or negative binomial offspring
#' distribution.
#' @return List of the offspring distribution parameter(s) with the mean
#' scaled by \code{1 - intvn_scale_r0}.
#' @details
#' `intvn_scale_r0()` scales the mean of the offspring distribution
#' by \eqn{1 - r0\_reduction} so that the new mean is given as:
#' \deqn{(1 - r0\_reduction) \times R_0,} where \eqn{R_0} is the
#' mean of the poisson and negative binomial distribution.
#'
#' @author James M. Azam
#' @export
intvn_scale_r0 <- function(r0_reduction, offspring_dist, pars_list) {
  # Intervention only works for pois and nbinom
  if (!offspring_dist %in% c("pois", "nbinom")) {
    stop(
      "`offspring_dist` must be one of c(\"pois\", \"nbinom\"), ",
      "if r0_reduction is specified."
    )
  }
  if (offspring_dist == "pois") {
    pars_list$lambda <- (1 - r0_reduction) * pars_list$lambda
  } else {
    pars_list$mu <- (1 - r0_reduction) * pars_list$mu
  }
  return(pars_list)
}
