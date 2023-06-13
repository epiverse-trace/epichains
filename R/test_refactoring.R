
source("./R/checks.R")
source("./R/helpers.R")
source("./R/epichains.R")
source("./R/simulate.r")


# try simulate_tree()
chains_tree <- simulate_tree(nchains = 10,
                                   serials_sampler = function(n) {rpois(n, 5)},
                                   offspring_sampler = "pois",
                                   lambda = 2,
                                   chain_stat_max = 10
                                   )


chains_tree
summary(chains_tree)
plot(chains_tree)

# try simulate_tree_from_pop()

chains_tree_from_pop <- simulate_tree_from_pop(
  pop = 100, offspring_sampler = "nbinom",
  mean_offspring = 0.5, disp_offspring = 1.1,
  serial_sampler = function(x) 3)

chains_tree_from_pop
summary(chains_tree_from_pop)
plot(chains_tree_from_pop)

# try chain_vec simulation
chains_vec <- simulate_vect(nchains = 10, offspring_sampler = "pois",
                             lambda = 2, chain_stat_max = 10
                             )

chains_vec
summary(chains_vec)
# plot(chains_vec) #expect error
