#' Determine and update the chain statistic being tracked
#'
#' @param stat_type Chain statistic (size/length) to update.
#' @param stat_latest The latest chain statistic vector to be updated.
#' @param n_offspring A vector of offspring per chain.
#' @return A vector of chain statistics (size/length).
#' @keywords internal
update_chain_stat <- function(stat_type, stat_latest, n_offspring) {
  if (stat_type == "size") {
    stat_latest <- stat_latest + n_offspring
  } else if (stat_type == "length") {
    stat_latest <- stat_latest + pmin(1, n_offspring)
  }

  return(stat_latest)
}

#' Return a function for calculating chain statistics
#'
#' @inheritParams simulate_tree
#'
#' @return a function for calculating chain statistics
#' @keywords internal
get_statistic_func <- function(chain_statistic) {
  func <- if (chain_statistic == "size") {
    rbinom_size
  } else if (chain_statistic == "length") {
    rgen_length
  }
  return(func)
}

#' Construct name of analytical function for estimating loglikelihood of
#' offspring
#'
#' @inheritParams simulate_tree
#'
#' @return an analytical offspring likelihood function
#' @keywords internal
construct_offspring_ll_name <- function(offspring_dist, chain_statistic) {
  ll_name <- paste(offspring_dist, chain_statistic, "ll", sep = "_")
  return(ll_name)
}
