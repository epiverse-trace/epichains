#' Determine and update the chain statistic being tracked
#'
#' @param stat_type
#' @param noffspring
#'
#' @return
#' @export
#' @keywords internal
#' @examples
update_chain_stat <- function(stat_type, stat_latest, n_offspring) {
  if (stat_type == "size") {
    stat_latest <- stat_latest + n_offspring
  } else if (stat_type == "length") {
    stat_latest <- stat_latest + pmin(1, n_offspring)
  }

  return(stat_latest)
}
