

#' Randomly assign groups
#'
#' (Brief description of the function here.)
#'
#' @param sample_size total number of people under observation.
#' @param prob0 vector probability of each possible outcome for the null group
#' @param prob1 vector probability of each possible outcome for the intervention group
#' @param K number of possible outcome values in the target variable
#' @param seed integer specifying the seed number
#'
#' @return list of group assignments
#' @export
#'
#'
#'
assign_groups <- function(sample_size, prob0, prob1, seed) {

  set.seed(seed)

  y <- factor(sample(0:1,sample_size,replace=TRUE))
  n_null <- sum(y==0)
  n_intervene <- sample_size-n_null
  x <- rep(1, sample_size)
  K <- length(prob0)

  x[y==0] <- sample(1:K, n_null, replace=TRUE, prob=prob0)
  x[y==1] <- sample(1:K, n_intervene, replace=TRUE, prob=prob1)
#
#   if( length(unique(x[y==0])) < K ) {
#     warning("Not all possible outcomes observed in the null group.")
#   }
#   if( length(unique(x[y==1])) < K ) {
#     warning("Not all possible outcomes observed in the intervention group.")
#   }

  list(y=y, x=x, n_null=n_null, n_intervene=n_intervene, sample_size=sample_size, K=K)
}
