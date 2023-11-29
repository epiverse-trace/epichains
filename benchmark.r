library("microbenchmark")
library("data.table")
devtools::load_all()
source("simulate_benchmark.r" )

args <- list(
  nchains = 10,
  statistic = "size",
  offspring_dist = "pois",
  serials_dist = function(n) rep(3, n),
  lambda = 2
)

microbenchmark(
  do.call(simulate_tree, c(args, stat_max = 100)),
  do.call(simulate_tree_do_call, c(args, stat_max = 100)),
  do.call(simulate_tree_rbindlist, c(args, stat_max = 100)),
  times = 100
)

microbenchmark(
  do.call(simulate_tree, c(args, stat_max = 1000)),
  do.call(simulate_tree_do_call, c(args, stat_max = 1000)),
  do.call(simulate_tree_rbindlist, c(args, stat_max = 1000)),
  times = 100
)

microbenchmark(
  do.call(simulate_tree, c(args, stat_max = 10000)),
  do.call(simulate_tree_do_call, c(args, stat_max = 10000)),
  do.call(simulate_tree_rbindlist, c(args, stat_max = 10000)),
  times = 100
)
