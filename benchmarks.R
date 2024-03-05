#' Benchmark of two approaches for obtaining chain summaries
#' 1. Simulate chains with `simulate_chains()` then run `summary()` on the
#' output to obtain an `<epichains_summary>` object OR
#' 2. Run `simualte_summary()` directly with the same parameters as
#' `simulate_chains()`

#' Load libraries
devtools::load_all()
library("bench")

# Shared arguments
shared_args <- list(
    index_cases = 10,
    statistic = "size",
    offspring_dist = rnbinom,
    stat_max = 1000,
    mu = 2,
    size = 0.2
)

#' Benchmark
summary_benchmarks <- bench::mark(
    sim_chains = {
        set.seed(32)
        out <- do.call(
            simulate_chains,
            shared_args
        )
        summary(out)
    },
    sim_summary = {
        set.seed(32)
        do.call(
            simulate_summary,
            shared_args
        )
    }
)

bnmark_plt <- plot(bnmarks)

ggplot2::ggsave(
    file.path("inst", "summary_benchmarks.png"),
    bnmark_plt
)
