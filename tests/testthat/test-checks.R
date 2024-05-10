# A function that passes all checks in .check_sim_args but returns an error
# message if the supplied arguments are invalid
.check_sim_args_default <- function(...) {
    default_args <- list(
        nsims = 10,
        statistic = "size",
        offspring_dist = rpois,
        stat_max = 10,
        pop = 10,
        percent_immune = 0.1
    )
    # Modify the default arguments with the user's arguments
    new_args <- modifyList(
        default_args,
        list(...)
    )
    # Run the check_sim_args function and capture the output
    out <- tryCatch(
        expr = {
            do.call(
                .check_sim_args,
                new_args
            )
        },
        error = function(e) {
            stop(e)
        }
    )
    # Return the output
    return(out)
}

# A function that passes all checks in `.check_time_args` but returns an error
# message if the supplied arguments are invalid
.check_time_args_default <- function(...) {
    default_args <- list(
        t0 = 0,
        tf_specified = FALSE, # tf is not specified but default tf = Inf below
        tf = Inf,
        generation_time = NULL
    )
    new_args <- modifyList(
        default_args,
        list(...)
    )
    out <- tryCatch(
        expr = {
            do.call(
                .check_time_args,
                new_args
            )
        },
        error = function(e) {
            stop(e)
        }
    )
    return(out)
}


test_that("Smaller checker functions work", {
    expect_error(
        .check_offspring_func_valid(rrpois),
        "not found"
    )
    expect_error(
        .check_generation_time_valid("a"),
        "Must be a function"
    )
    expect_error(
        .check_generation_time_valid(function(x) rep("a", 10)),
        "numeric"
    )
    expect_error(
        .check_generation_time_valid(function(x) 3),
        "Must have length"
    )
    expect_no_error(
        .check_statistic_args(
            statistic = "size",
            stat_max = 10
        )
    )
})

test_that(".check_sim_args() returns errors", {
    # Checks with .check_sim_args
    expect_no_error(
        .check_sim_args_default()
    )
    # nsims must be >= 1
    expect_error(
        .check_sim_args_default(
            nsims = 0
        ),
        "Must be >= 1."
    )
    # statistic can only be "size" or "length"
    expect_error(
        .check_sim_args_default(
            statistic = "duration"
        ),
        "Must be element of set \\{'size','length'\\}"
    )
    # offspring_dist must be a function
    expect_error(
        .check_sim_args_default(
            offspring_dist = "rpois"
        ),
        "Must be a function, not 'character'"
    )
    # offspring_dist must be a known function (in the environment)
    expect_error(
        .check_sim_args_default(
            offspring_dist = r
        ),
        "object 'r' not found"
    )
    # stat_max must be >= 1
    expect_error(
        .check_sim_args_default(
            stat_max = 0
        ),
        "Assertion failed."
    )
    # pop cannot be negative
    expect_error(
        .check_sim_args_default(
            pop = -1
        ),
        "Element 1 is not >= 1."
    )
    # percent_immune must be in between 0 and 1
    expect_error(
        .check_sim_args_default(
            percent_immune = 1.1
        ),
        "Element 1 is not <= 1."
    )
})

test_that(".check_time_args() returns errors", {
    # Checks with .check_time_args
    expect_no_error(
        .check_time_args_default()
    )
    # t0 cannot be negative
    expect_error(
        .check_time_args_default(
            t0 = -1
        ),
        "Element 1 is not >= 0."
    )
    # tf cannot be negative
    expect_error(
        .check_time_args_default(
            tf = -1
        ),
        "Element 1 is not >= 0."
    )
    # If tf is specified, generation_time must be specified too
    expect_error(
        .check_time_args_default(
            tf_specified = TRUE,
            tf = 10
        ),
        "If `tf` is specified, `generation_time` must be specified too."
    )
})
