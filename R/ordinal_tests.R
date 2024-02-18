
#' Ordinal outcome tests
#'
#' A metafunction that runs the statistical tests listed below, and returns the p-values as a named vector.
#'
#' * `stats::wilcox.test()`
#' * `stats::fisher.test()`
#' * `stats::chisq.test(correct = FALSE)`
#' * `stats::chisq.test(correct = TRUE)`
#' * `rms::lrm()`
#' * `stats.kruskal.test()`
#' * `coin::independence_test(ytrafo = coin::rank_trafo)`
#'
#' @param x Group one
#' @param y Group two
#' @param ... Placeholder for additional arguments to functions
#'
#' @return A named vector of probabilities for each test
#'
#'
#' @export
#'
ordinal_tests <- function(x, y, ...) {

  c(
    wilcox = stats::wilcox.test(x[y==0],x[y==1])[["p.value"]],
    fisher = stats::fisher.test(x,y,simulate.p.value=FALSE, workspace = 2e7)[["p.value"]],
    chi_sq_false = stats::chisq.test(x, y, correct=FALSE)[["p.value"]],
    chi_sq_true = stats::chisq.test(x, y, correct=TRUE)[["p.value"]],
    lrm = rms::lrm(x~y)$stats[["P"]],
    kruskal = stats::kruskal.test(x~y)[["p.value"]],
    coinasymp = coin::pvalue(coin::independence_test(x~y, ytrafo=coin::rank_trafo))
  )

}
