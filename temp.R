


  # Helper function to run the simulation logic for each sample size
  run_simulation_logic <- function(sample_size_nested, sample_prob, prob0, prob1, niter, included = "all",
                                   .rng_kind = NULL, .rng_normal_kind = NULL, .rng_sample_kind = NULL) {
    assert_that(
      length(prob0) == length(prob1),
      msg = "prob0 and prob1 must have the same length"
    )
    assert_that(
      near(sum(prob0), 1),
      msg = "prob0 must sum to 1"
    )
    assert_that(
      near(sum(prob1), 1),
      msg = "prob1 must sum to 1"
    )
    assert_that(
      length(included) > 0,
      msg = "No tests included in the simulation. Please include at least one test."
    )
    assert_that(
      all(included %in% c(
        "all", "Wilcoxon", "Fisher", "Chi Squared (No Correction)",
        "Chi Squared (Correction)", "Prop. Odds", "Coin Indep. Test"
      )),
      msg = "included tests must be any of 'all', 'Wilcoxon', 'Fisher', 'Chi Squared (No Correction)',
        'Chi Squared (Correction)', 'Prop. Odds', 'Coin Indep. Test'"
    )

    if ("all" %in% included) {
      included <- c(
        "Wilcoxon", "Fisher", "Chi Squared (No Correction)",
        "Chi Squared (Correction)", "Prop. Odds", "Coin Indep. Test"
      )
    }

    K <- length(prob0)
    p_values <- matrix(NA, niter, length(included))
    colnames(p_values) <- included

    initial_groups <- lapply(1:niter, function(x) {
      assign_groups(
        sample_size = sample_size_nested,
        sample_prob = sample_prob,
        prob0 = prob0, prob1 = prob1,
        seed = x,
        .rng_kind = .rng_kind,
        .rng_normal_kind = .rng_normal_kind,
        .rng_sample_kind = .rng_sample_kind
      )
    })

    p_values <- initial_groups %>%
      sapply(., function(x) ordinal_tests(x[["x"]], x[["y"]], included = included)) %>%
      matrix(byrow = TRUE, nrow = niter)

    colnames(p_values) <- included

    initial_groups_formatted <- lapply(initial_groups, function(groups) {
      tibble(
        y = list(groups[["y"]]), x = list(groups[["x"]]),
        n_null = groups[["n_null"]], n_intervene = groups[["n_intervene"]],
        sample_size = groups[["sample_size"]], K = groups[["K"]]
      )
    }) %>%
      bind_rows() %>%
      mutate(run = row_number(), .before = .data$y)

    if (shiny::isRunning()) {
      incProgress(
        1 / (max(sample_size) - min(sample_size)),
        detail = paste("Sample size", sample_size_nested, "completed.")
      )
    }

    return(bind_cols(p_values, initial_groups_formatted))
  }

  # Initialize background processes for each sample size
  processes <- lapply(100:300, function(x) {
    callr::r_bg(run_simulation_logic, args = list(x, sample_prob = c(0.5, 0.5), prob0 = c(0.5, 0.5),
                                                  prob1 = c(0.5, 0.5), niter = 100, included = "all"))
  })

  processes[[1]]$kill()




proc2 <- callr::process$new(
  "ordinalsimr::run_simulations", args = 'list(sample_size = 100:300, sample_prob = c(0.5, 0.5), prob0 = c(0.5, 0.5),
                                    prob1 = c(0.5, 0.5), niter = 100, included = "all")'
)


proc2$is_alive()





