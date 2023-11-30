library("bench")
library("data.table")
devtools::load_all()
source("simulate_benchmark.r")

args <- list(
  nchains = 10,
  statistic = "size",
  offspring_dist = "pois",
  serials_dist = function(n) rep(3, n),
  lambda = 2
)

bnmk <- bench::press(
  stat_max = c(1E2, 1E3, 1E4),
  {
    bench::mark(
      simulate_tree = do.call(simulate_tree, c(args, stat_max = stat_max)),
      simulate_summary = do.call(simulate_summary, c(modifyList(args, list(serials_dist = NULL)), stat_max = stat_max)),
      simulate_tree_do_call = do.call(simulate_tree_do_call, c(args, stat_max = stat_max)),
      simulate_tree_rbindlist = do.call(simulate_tree_rbindlist, c(args, stat_max = stat_max)),
      check = FALSE
    )
  }
)

ggplot2::autoplot(bnmk)
