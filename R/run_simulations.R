#' Run Simulations
#'
#' @param sample_size Total number of trial participants
#' @param sample_prob a vector of probability weights for obtaining the elements of the vector being sampled.
#' @param prob0 Vector of probabilities for control group
#' @param prob1 Vector of probabilities for intervention group
#' @param .rng_kind seeding info passed to withr::with_seed
#' @param .rng_normal_kind seeding info passed to withr::with_seed
#' @param .rng_sample_kind seeding info passed to withr::with_seed
#' @param niter Number of simulation iterations to complete#'
#' @param included a character vector of the tests to be included. Default is "all"
#'
#' @return a list of lists; sub-list elements include `p_values` which is a matrix of p values for tests at each iteration, and `initial_groups` which is the group assignment information for each iteration
#'
#' @import assertthat
#'
#' @export
#'
#' @examples
#' run_simulations(
#'   sample_size = c(40, 50, 60),
#'   sample_prob = c(0.5, 0.5),
#'   prob0 = c(0.1, 0.2, 0.3, 0.4),
#'   prob1 = c(0.6, 0.2, 0.1, 0.1),
#'   niter = 40
#' )
#'
run_simulations <- function(sample_size, sample_prob, prob0, prob1, niter, included = "all",
                            .rng_kind = NULL, .rng_normal_kind = NULL, .rng_sample_kind = NULL) {
  # Check equal vector lengths
  assert_that(
    length(prob0) == length(prob1),
    msg = "prob0 and prob1 must have the same length"
  )
  # Check probabilities for both groups sum to 1
  assertthat::assert_that(
    near(sum(prob0), 1),
    msg = "prob0 must sum to 1"
  )
  assertthat::assert_that(
    near(sum(prob1), 1),
    msg = "prob0 must sum to 1"
  )
  # Check included argument
  assertthat::assert_that(
    length(included) > 0,
    msg = "No tests included in the simulation. Please include at least one test."
  )
  assertthat::assert_that(
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

  # c(
  #   "Wilcoxon", "Fisher", "Chi Squared\n(No Correction)",
  #   "Chi Squared\n(Correction)", "Prop. Odds", "Coin Indep. Test"
  # )


  lapply(sample_size, function(x) {
    sample_size_nested <- x
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


    return(sim_results_table = bind_cols(p_values, initial_groups_formatted))
  }) %>%
    rlang::set_names(paste0("sample_size_", sample_size))
}






#' Run Simulations in Background
#'
#' This function wraps the run_simulations function, and runs it in the background using callr::r_bg while also writing the sample size to a temporary file for progress bar tracking.
#'
#' @param sample_size Total number of trial participants
#' @param sample_prob a vector of probability weights for obtaining the elements of the vector being sampled.
#' @param prob0 Vector of probabilities for control group
#' @param prob1 Vector of probabilities for intervention group
#' @param .rng_kind seeding info passed to withr::with_seed
#' @param .rng_normal_kind seeding info passed to withr::with_seed
#' @param .rng_sample_kind seeding info passed to withr::with_seed
#' @param niter Number of simulation iterations to complete#'
#' @param included a character vector of the tests to be included. Default is "all"
#' @param tempfile path to a temporary file to write the sample size to
#'
#' @noRd
#' @returns a list of lists; sub-list elements include `p_values` which is a matrix of p values for tests at each iteration, and `initial_groups` which is the group assignment information for each iteration
#'
run_simulations_in_background <- function(sample_size, sample_prob, prob0, prob1, niter, included = "all",
                                          .rng_kind = NULL, .rng_normal_kind = NULL, .rng_sample_kind = NULL,
                                          tempfile = NULL) {

  callr::r_bg(function(sample_size, sample_prob, prob0, prob1, niter, included,
                       .rng_kind, .rng_normal_kind, .rng_sample_kind, tempfile) {


      run_simulation_wrapper <- function(sample_size, sample_prob, prob0, prob1, niter, included,
                                         .rng_kind, .rng_normal_kind, .rng_sample_kind, tempfile) {
        lapply(sample_size, function(x) {
          try(writeLines(as.character(x), con = tempfile))
          run_simulations(x, sample_prob = sample_prob, prob0 = prob0, prob1 = prob1, niter = niter, included = included,
                                       .rng_kind = .rng_kind, .rng_normal_kind = .rng_normal_kind, .rng_sample_kind = .rng_sample_kind
          )
        }) |>
          unlist(recursive = FALSE)
      }

      run_simulation_wrapper(sample_size, sample_prob, prob0, prob1, niter, included,
                             .rng_kind, .rng_normal_kind, .rng_sample_kind, tempfile)

    },
    args = list(sample_size, sample_prob, prob0, prob1, niter, included,
                 .rng_kind, .rng_normal_kind, .rng_sample_kind, tempfile),
    package = TRUE
  )

}
