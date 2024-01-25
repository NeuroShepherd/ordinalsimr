

run_simulations <- function(sample_size, prob0, prob1, niter) {

  # Check equal vector lengths
  assert_that( length(prob0) == length(prob1) )
  # Check probabilities for both groups sum to 1
  assert_that( dplyr::near(sum(prob0), 1) )
  assert_that( dplyr::near(sum(prob1), 1) )


  K <- length(prob0)
  p_values <- matrix(NA,niter,7)
  colnames(p_values) <- c("wilcox", "fisher", "chisqFALSE", "chisqTRUE",
    "lrm", "coinasymp","coinexact")


  initial_groups <- list()

  for (i in 1:niter) {
    initial_groups[[i]] <- assign_groups(sample_size = sample_size,
                                    prob0 = prob0, prob1 = prob1,
                                    seed = i)

    p_values[i, ] <- ordinal_tests(x = initial_groups[[i]]$x,
                                   y = initial_groups[[i]]$y)
  }

  return(list(p_values = p_values, initial_groups = initial_groups))

}
