#' Reduce the mean of the offspring distribution
#'
#' @description
#' `intvn_reduce_mean()` is a helper for the \code{simulate_*()} functions. It
#' reduces/scales the mean of the offspring distribution in order to
#' mimic the impact of a population-level intervention. Currently, it can only
#' handle the poisson and negative binomial distributions and errors when other
#' offspring distributions are specified alongside the `intvn_mean_reduction`
#' argument.
#'
#' @inheritParams simulate_tree
#' @param intvn_mean_reduction A number between 0
#' and 1 for scaling/reducing the mean of `offspring_dist`. Serves as
#' population-level intervention. `intvn_mean_reduction` = 0
#' implies no intervention impact and `intvn_mean_reduction` = 1 implies full
#' impact.
#' @param pars_list Parameter(s) for poisson or negative binomial offspring
#' distribution.
#' @return List of the offspring distribution parameter(s) with the mean
#' scaled by \code{1 - intvn_mean_reduction}.
#' @details
#' `intvn_reduce_mean()` scales the mean of the offspring distribution
#' by \eqn{1 - {\sf intvn\_mean\_reduction}} so that the new mean is given as:
#' \deqn{(1 - {\sf intvn\_mean\_reduction}) \times {\sf mean,}} This
#' scaling when applied to the poisson and negative binomial offspring
#' distributions corresponds to the population-level reduction of R0 as
#' described in Lloyd-Smith et al, (2005). `intvn_reduce_mean()` is therefore
#' only implemented for the aforementioned distributions and errors when other
#' offspring distributions are specified along with the `intvn_mean_reduction`
#' argument in the \code{simulate_*()} functions.
#'
#' @author James M. Azam
#' @keywords internal
#' @references Lloyd-Smith, J., Schreiber, S., Kopp, P. et al. Superspreading
#' and the effect of individual variation on disease emergence. Nature 438,
#' 355–359 (2005). \doi{10.1038/nature04153}
intvn_reduce_mean <- function(intvn_mean_reduction, offspring_dist, pars_list) {
  # Intervention only works for pois and nbinom
  if (!offspring_dist %in% c("pois", "nbinom")) {
    stop(
      "`offspring_dist` must be one of c(\"pois\", \"nbinom\"), ",
      "if intvn_mean_reduction is specified."
    )
  }
  if (offspring_dist == "pois") {
    pars_list$lambda <- (1 - intvn_mean_reduction) * pars_list$lambda
  } else {
    pars_list$mu <- (1 - intvn_mean_reduction) * pars_list$mu
  }
  return(pars_list)
}
