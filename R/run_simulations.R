
#' Run Simulations
#'
#' @param sample_size Total number of trial participants
#' @param sample_prob a vector of probability weights for obtaining the elements of the vector being sampled.
#' @param prob0 Vector of probabilities for control group
#' @param prob1 Vector of probabilities for intervention group
#' @param niter Number of simulation iterations to complete#'
#' @return list with elements `p_values` which is a matrix of p values for tests at each iteration, and `initial_groups` which is the group assignment information for each iteration
#'
#' @import assertthat
#'
#' @export
#'
#'
run_simulations <- function(sample_size, sample_prob, prob0, prob1, niter) {

  # Check equal vector lengths
  assert_that( length(prob0) == length(prob1) )
  # Check probabilities for both groups sum to 1
  assert_that( dplyr::near(sum(prob0), 1), msg = "Probability for Group 1 does not sum to 1." )
  assert_that( dplyr::near(sum(prob1), 1), msg = "Probability for Group 2 does not sum to 1" )


  K <- length(prob0)
  p_values <- matrix(NA,niter,7)
  colnames(p_values) <- c("Wilcoxon", "Fisher", "Chi Squared\n(No Correction)", "Chi Squared\n(Correction)",
    "Logistic Reg.", "Kruskal-Wallis", "Coin Indep. Test")


  initial_groups <- list()

  for (i in 1:niter) {
    initial_groups[[i]] <- assign_groups(sample_size = sample_size,
                                         sample_prob = sample_prob,
                                         prob0 = prob0, prob1 = prob1,
                                         seed = i)

    p_values[i, ] <- ordinal_tests(x = initial_groups[[i]]$x,
                                   y = initial_groups[[i]]$y)
  }

  return(list(p_values = p_values, initial_groups = initial_groups))

}
