#' Ordinal outcome tests
#'
#' A metafunction that runs the statistical tests listed below, and returns the p-values as a named vector.
#'
#' \itemize{
#'  \item{stats::wilcox.test()}
#'  \item{stats::fisher.test()}
#'  \item{stats::chisq.test(correct = FALSE)}
#'  \item{stats::chisq.test(correct = TRUE)}
#'  \item{rms::lrm()}
#'  \item{coin::independence_test(ytrafo = coin::rank_trafo)}
#'  }
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
    Wilcoxon = stats::wilcox.test(x[y == 0], x[y == 1])[["p.value"]],
    Fisher = stats::fisher.test(x, y, simulate.p.value = FALSE, workspace = 2e7)[["p.value"]],
    "Chi Squared\n(No Correction)" = stats::chisq.test(x, y, correct = FALSE)[["p.value"]],
    "Chi Squared\n(Correction)" = stats::chisq.test(x, y, correct = TRUE)[["p.value"]],
    "Prop. Odds" = rms::lrm(x ~ y)$stats[["P"]],
    "Coin Indep. Test" = coin::pvalue(coin::independence_test(x ~ y, ytrafo = coin::rank_trafo))
  )
}
