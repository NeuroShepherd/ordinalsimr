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
#'   niter = 100
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


  K <- length(prob0)
  p_values <- matrix(NA, niter, 6)
  colnames(p_values) <- c(
    "Wilcoxon", "Fisher", "Chi Squared\n(No Correction)",
    "Chi Squared\n(Correction)", "Prop. Odds", "Coin Indep. Test"
  )


  purrr::map(sample_size,
    ~ {
      sample_size_nested <- .x
      initial_groups <- purrr::map(1:niter, ~{
        assign_groups(
          sample_size = sample_size_nested,
          sample_prob = sample_prob,
          prob0 = prob0, prob1 = prob1,
          seed = .x,
          .rng_kind = .rng_kind,
          .rng_normal_kind = .rng_normal_kind,
          .rng_sample_kind = .rng_sample_kind
        )
      }
      )

      p_values <- initial_groups %>%
        sapply(., function(x) ordinal_tests(x[["x"]], x[["y"]], included = included)) %>%
        t()

      initial_groups_formatted <- initial_groups %>%
        purrr::map_df(~ tibble(
          y = list(.x[["y"]]), x = list(.x[["x"]]),
          n_null = .x[["n_null"]], n_intervene = .x[["n_intervene"]],
          sample_size = .x[["sample_size"]], K = .x[["K"]]
        )) %>%
        mutate(run = row_number(), .before = y)

      return(sim_results_table = bind_cols(p_values, initial_groups_formatted))
    },
    .progress = list(
      caller = environment(),
      format = "Running {niter} iterations on {length(sample_size)} sample sizes. Progress: {cli::pb_bar} {cli::pb_percent} {cli::pb_eta}"
    )
  ) %>%
    purrr::set_names(paste0("sample_size_",sample_size))
}
