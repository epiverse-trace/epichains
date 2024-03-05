# Benchmark of two approaches for obtaining chain summaries
# 1. Simulate chains with `simulate_chains()` then run `summary()` on the
# output to obtain an `<epichains_summary>` object OR
# 2. Run `simualte_summary()` directly with the same parameters as
# `simulate_chains()`

# Load libraries
library(epichains)
library(bench)

# Benchmark
summary_benchmarks <- bench::mark(
    sim_chains = {
        set.seed(32)
        out <- simulate_chains(
            index_cases = 10,
            statistic = "size",
            offspring_dist = rnbinom,
            stat_max = 1000,
            mu = 2,
            size = 0.2
        )
        summary(out)
    },
    sim_summary = {
        set.seed(32)
        simulate_summary(
            index_cases = 10,
            statistic = "size",
            offspring_dist = rnbinom,
            stat_max = 1000,
            mu = 2,
            size = 0.2
        )
    }
)

bnmark_plt <- plot(bnmarks)

ggplot2::ggsave("./inst/summary_benchmarks.png", bnmark_plt)


