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
    Wilcoxon = tryCatch(
      {stats::wilcox.test(x[y == 0], x[y == 1])[["p.value"]]},
      error = function(e) {NA_real_}
      ),
    Fisher = tryCatch(
      {stats::fisher.test(x, y, simulate.p.value = FALSE, workspace = 2e7)[["p.value"]]},
      error = function(e) {NA_real_}
    ),
    "Chi Squared\n(No Correction)" = tryCatch(
      {stats::chisq.test(x, y, correct = FALSE)[["p.value"]]},
      error = function(e) {NA_real_}
    ),
    "Chi Squared\n(Correction)" = tryCatch(
      {stats::chisq.test(x, y, correct = TRUE)[["p.value"]]},
      error = function(e) {NA_real_}
    ),
    "Prop. Odds" = tryCatch(
      {rms::lrm(x ~ y)$stats[["P"]]},
      error = function(e) {NA_real_}
    ),
    "Coin Indep. Test" = tryCatch(
      {coin::pvalue(coin::independence_test(x ~ y, ytrafo = coin::rank_trafo))},
      error = function(e) {NA_real_}
    )
  )
}


