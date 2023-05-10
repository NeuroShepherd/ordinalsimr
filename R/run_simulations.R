

run_simulations <- function(sample_size, settings, niter) {

  prob0 <- settings$prob0
  prob1 <- settings$prob1
  K <- length(prob0)

  # Check equal vector lengths
  if (length(prob0) != length(prob1)) {
    stop("Probability vectors must be of equal length")
  }

  # Check probabilities for both groups sum to 1
  if (sum(prob0) != 1){
    stop("Probability vector of the null group must sum to 1")
  }
  if (sum(prob1) != 1){
    stop("Probability vector of the interventional group must sum to 1")
  }



  p_values <- matrix(NA,niter,7)
  colnames(p_values) <- c(
    # "maxselA",
    # "maxselE",
    "wilcox",
    "fisher",
    "chisqFALSE",
    "chisqTRUE",
    "lrm",
    "coinasymp",
    "coinexact"
    )


  for (i in 1:niter) {

    initial_groups <- assign_groups(sample_size = sample_size,
                                    prob0 = prob0, prob1 = prob1,
                                    K = K, seed = i)

    # Need to write a stop or error condition for really small values of n just to be safe. For some seeds, values are only assigned to one of the possible groups (control or treatment, var `y`) which doesn't play nice with, at minimum, the chi.square() function (but also probably is true for any nominal outcome var). I guess write a check that there's at least one person in each of the two groups?

    # Only including this because rms::lrm doesn't play nice with the object$object notation
    # x <- initial_groups$x
    # y <- initial_groups$y
    #
    # p_values[i,1] <- stats::wilcox.test(x[y==0],x[y==1])$p.value
    # p_values[i,2] <- stats::fisher.test(x,y,simulate.p.value=TRUE)$p.value
    # p_values[i,3] <- stats::chisq.test(x, y, correct=FALSE)$p.value
    # p_values[i,4] <- stats::chisq.test(x, y, correct=TRUE)$p.value
    # p_values[i,5] <- rms::lrm(x~y)$stats[5]
    # p_values[i,6] <- coin::pvalue(coin::independence_test(x~y, ytrafo=coin::rank_trafo))
    # # the exact independence test is really slow, and Anne-Laure's code excludes it for n>100. I should do the same. I think this was actually the real bottleneck in my code.
    # p_values[i,7] <- coin::pvalue(coin::independence_test(x~y, ytrafo=coin::rank_trafo, distribution="exact"))

    p_values <- ordinal_tests(x = initial_groups$x, y = initial_groups$y,
                               sample_size = initial_groups$sample_size,
                               K = initial_groups$K)

  }
  p_values
  # initial_groups
}
