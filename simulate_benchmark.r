simulate_tree_do_call <- function(nchains, statistic = c("size", "length"),
                          offspring_dist, stat_max = Inf,
                          serials_dist, t0 = 0,
                          tf = Inf, ...) {
  statistic <- match.arg(statistic)

  # Input checking
  check_nchains_valid(nchains = nchains)
  checkmate::assert_character(statistic)

  # check that offspring is properly specified
  check_offspring_valid(offspring_dist)

  # check that offspring function exists in base R
  roffspring_name <- paste0("r", offspring_dist)
  check_offspring_func_valid(roffspring_name)

  checkmate::assert_number(
    stat_max, lower = 0
  )

  if (!missing(serials_dist)) {
    check_serial_valid(serials_dist)
  }
  checkmate::assert_numeric(
    t0, lower = 0, finite = TRUE
  )
  checkmate::assert_number(
    tf, lower = 0
  )

  # Gather offspring distribution parameters
  pars <- list(...)

  if (!missing(serials_dist)) {
    check_serial_valid(serials_dist)
  } else if (!missing(tf)) {
    stop("If `tf` is specified, `serials_dist` must be specified too.")
  }

  # Initialisations
  stat_track <- rep(1, nchains) # track length or size (depending on `statistic`) #nolint
  n_offspring <- rep(1, nchains) # current number of offspring
  sim <- seq_len(nchains) # track chains that are still being simulated
  ancestor_ids <- rep(1, nchains) # all chains start in generation 1

  # initialise data frame to hold the transmission trees
  generation <- 1L
  tree_df <- list(data.frame(
    chain_id = seq_len(nchains),
    sim_id = 1L,
    ancestor = NA_integer_,
    generation = generation
  ))

  if (!missing(serials_dist)) {
    tree_df[[generation]]$time <- t0
    times <- tree_df[[generation]]$time
  }

  # next, simulate n chains
  while (length(sim) > 0) {
    # simulate next generation
    next_gen <- do.call(
      get(roffspring_name),
      c(
        list(n = sum(n_offspring[sim])),
        pars
      )
    )
    if (any(next_gen %% 1 > 0)) {
      stop("Offspring distribution must return integers")
    }

    # record indices corresponding to the number of offspring
    indices <- rep(sim, n_offspring[sim])

    # initialise placeholder for the number of offspring
    n_offspring <- rep(0, nchains)
    # assign offspring sum to indices still being simulated
    n_offspring[sim] <- tapply(next_gen, indices, sum)

    # track size/length
    stat_track <- update_chain_stat(
      stat_type = statistic,
      stat_latest = stat_track,
      n_offspring = n_offspring
    )

    # record times/ancestors
    if (sum(n_offspring[sim]) > 0) {
      ancestors <- rep(ancestor_ids, next_gen)
      current_max_id <- unname(tapply(ancestor_ids, indices, max))
      indices <- rep(sim, n_offspring[sim])

      # create new ids
      ids <- rep(current_max_id, n_offspring[sim]) +
        unlist(lapply(n_offspring[sim], seq_len))

      # increment the generation
      generation <- generation + 1L

      # store new simulation results
      tree_df[[generation]] <-
        data.frame(
          chain_id = indices,
          sim_id = ids,
          ancestor = ancestors,
          generation = generation
        )

      # if a serial interval model/function was specified, use it
      # to generate serial intervals for the cases
      if (!missing(serials_dist)) {
        times <- rep(times, next_gen) + serials_dist(sum(n_offspring))
        current_min_time <- unname(tapply(times, indices, min))
        tree_df[[generation]]$time <- times
      }
    }

    ## only continue to simulate chains that have offspring and aren't of
    ## the specified maximum size/length
    sim <- which(n_offspring > 0 & stat_track < stat_max)
    if (length(sim) > 0) {
      if (!missing(serials_dist)) {
        ## only continue to simulate chains that don't go beyond tf
        sim <- intersect(sim, unique(indices)[current_min_time < tf])
      }
      if (!missing(serials_dist)) {
        times <- times[indices %in% sim]
      }
      ancestor_ids <- ids[indices %in% sim]
    }
  }

  ## bind results together
  tree_df <- do.call(rbind, tree_df)

  if (!missing(tf)) {
    tree_df <- tree_df[tree_df$time < tf, ]
  }

  # sort by sim_id and ancestor
  tree_df <- tree_df[order(tree_df$sim_id, tree_df$ancestor), ]

  structure(
    tree_df,
    chains = nchains,
    chain_type = "chains_tree",
    rownames = NULL,
    track_pop = FALSE,
    class = c("epichains", "data.frame")
  )
}

simulate_tree_rbindlist <- function(nchains, statistic = c("size", "length"),
                          offspring_dist, stat_max = Inf,
                          serials_dist, t0 = 0,
                          tf = Inf, ...) {
  statistic <- match.arg(statistic)

  # Input checking
  check_nchains_valid(nchains = nchains)
  checkmate::assert_character(statistic)

  # check that offspring is properly specified
  check_offspring_valid(offspring_dist)

  # check that offspring function exists in base R
  roffspring_name <- paste0("r", offspring_dist)
  check_offspring_func_valid(roffspring_name)

  checkmate::assert_number(
    stat_max, lower = 0
  )

  if (!missing(serials_dist)) {
    check_serial_valid(serials_dist)
  }
  checkmate::assert_numeric(
    t0, lower = 0, finite = TRUE
  )
  checkmate::assert_number(
    tf, lower = 0
  )

  # Gather offspring distribution parameters
  pars <- list(...)

  if (!missing(serials_dist)) {
    check_serial_valid(serials_dist)
  } else if (!missing(tf)) {
    stop("If `tf` is specified, `serials_dist` must be specified too.")
  }

  # Initialisations
  stat_track <- rep(1, nchains) # track length or size (depending on `statistic`) #nolint
  n_offspring <- rep(1, nchains) # current number of offspring
  sim <- seq_len(nchains) # track chains that are still being simulated
  ancestor_ids <- rep(1, nchains) # all chains start in generation 1

  # initialise data frame to hold the transmission trees
  generation <- 1L
  tree_df <- list(data.table(
    chain_id = seq_len(nchains),
    sim_id = 1L,
    ancestor = NA_integer_,
    generation = generation
  ))

  if (!missing(serials_dist)) {
    tree_df[[generation]]$time <- t0
    times <- tree_df[[generation]]$time
  }

  # next, simulate n chains
  while (length(sim) > 0) {
    # simulate next generation
    next_gen <- do.call(
      get(roffspring_name),
      c(
        list(n = sum(n_offspring[sim])),
        pars
      )
    )
    if (any(next_gen %% 1 > 0)) {
      stop("Offspring distribution must return integers")
    }

    # record indices corresponding to the number of offspring
    indices <- rep(sim, n_offspring[sim])

    # initialise placeholder for the number of offspring
    n_offspring <- rep(0, nchains)
    # assign offspring sum to indices still being simulated
    n_offspring[sim] <- tapply(next_gen, indices, sum)

    # track size/length
    stat_track <- update_chain_stat(
      stat_type = statistic,
      stat_latest = stat_track,
      n_offspring = n_offspring
    )

    # record times/ancestors
    if (sum(n_offspring[sim]) > 0) {
      ancestors <- rep(ancestor_ids, next_gen)
      current_max_id <- unname(tapply(ancestor_ids, indices, max))
      indices <- rep(sim, n_offspring[sim])

      # create new ids
      ids <- rep(current_max_id, n_offspring[sim]) +
        unlist(lapply(n_offspring[sim], seq_len))

      # increment the generation
      generation <- generation + 1L

      # store new simulation results
      tree_df[[generation]] <-
        data.table(
          chain_id = indices,
          sim_id = ids,
          ancestor = ancestors,
          generation = generation
        )

      # if a serial interval model/function was specified, use it
      # to generate serial intervals for the cases
      if (!missing(serials_dist)) {
        times <- rep(times, next_gen) + serials_dist(sum(n_offspring))
        current_min_time <- unname(tapply(times, indices, min))
        tree_df[[generation]]$time <- times
      }
    }

    ## only continue to simulate chains that have offspring and aren't of
    ## the specified maximum size/length
    sim <- which(n_offspring > 0 & stat_track < stat_max)
    if (length(sim) > 0) {
      if (!missing(serials_dist)) {
        ## only continue to simulate chains that don't go beyond tf
        sim <- intersect(sim, unique(indices)[current_min_time < tf])
      }
      if (!missing(serials_dist)) {
        times <- times[indices %in% sim]
      }
      ancestor_ids <- ids[indices %in% sim]
    }
  }

  ## bind results together
  tree_df <- rbindlist(tree_df)

  if (!missing(tf)) {
    tree_df <- tree_df[tree_df$time < tf, ]
  }

  # sort by sim_id and ancestor
  tree_df <- tree_df[order(tree_df$sim_id, tree_df$ancestor), ]

  structure(
    tree_df,
    chains = nchains,
    chain_type = "chains_tree",
    rownames = NULL,
    track_pop = FALSE,
    class = c("epichains", "data.frame")
  )
}
