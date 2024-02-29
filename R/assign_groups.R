

#' Randomly assign groups
#'
#' (Brief description of the function here.)
#'
#' @param sample_size total number of people under observation.
#' @param sample_prob a vector of probability weights for obtaining the elements of the vector being sampled.
#' @param prob0 vector probability of each possible outcome for the null group
#' @param prob1 vector probability of each possible outcome for the intervention group
#' @param seed integer specifying the seed number
#' @param .rng_kind seeding info passed to withr::with_seed
#' @param .rng_normal_kind seeding info passed to withr::with_seed
#' @param .rng_sample_kind seeding info passed to withr::with_seed
#'
#' @return list of group assignments
#' @export
#' @importFrom withr with_seed
#'
#'
#'
assign_groups <- function(sample_size, sample_prob, prob0, prob1, seed,
                          .rng_kind = NULL, .rng_normal_kind = NULL, .rng_sample_kind = NULL) {

  withr::with_seed(seed, {

  y <- factor(sample(x = 0:1, size = sample_size, replace = TRUE, prob = sample_prob))
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

  list(y=y, x=x, n_null=n_null, n_intervene=n_intervene, sample_size=sample_size, K=K,
       .rng_kind = .rng_kind, .rng_normal_kind = .rng_normal_kind, .rng_sample_kind = .rng_sample_kind)
  },
  .rng_kind = .rng_kind,
  .rng_normal_kind = .rng_normal_kind,
  .rng_sample_kind = .rng_sample_kind
  )

}
