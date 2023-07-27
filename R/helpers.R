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


#' Get offspring sampling function that takes into account susceptible
#' depletion
#'
#' @param n Number of items to sample
#' @param susc Susceptible population size (calculated
#' inside \code{\link{simulate_tree_from_pop}}  as pop - initial_immune)
#' @inheritParams simulate_tree_from_pop
#'
#' @return An offspring sampling function
#' @keywords internal
get_offspring_func <- function(offspring_sampler, n, susc, pop,
                               mean_offspring, disp_offspring = NULL) {
  if (offspring_sampler == "nbinom") {
    function(n, susc, pop, mean_offspring, disp_offspring) {
      ## get distribution params from mean and dispersion
      new_mn <- mean_offspring * susc / pop ## apply susceptibility
      size <- new_mn / (disp_offspring - 1)

      ## using a right truncated nbinom distribution
      ## to avoid more cases than susceptibles
      truncdist::rtrunc(
        n,
        spec = "nbinom",
        b = susc,
        mu = new_mn,
        size = size
      )
    }
  } else if (offspring_sampler == "pois") {
    function(n, susc, pop, mean_offspring, disp_offspring) {
      truncdist::rtrunc(
        n,
        spec = "pois",
        lambda = mean_offspring * susc / pop,
        b = susc
      )
    }
  } else {
    stop("offspring_sampler must either be 'pois' or 'nbinom'")
  }
}



#' Return a function for calculating chain statistics
#'
#' @inheritParams simulate_tree
#'
#' @return a function for calculating chain statistics
#' @keywords internal
get_chain_statistic_func <- function(chain_statistic) {
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
construct_offspring_ll_name <- function(offspring_sampler, chain_statistic) {
  ll_name <- paste(offspring_sampler, chain_statistic, "ll", sep = "_")
  return(ll_name)
}
