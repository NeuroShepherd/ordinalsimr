
#' Ordinal outcome tests
#'
#'  (Description. Mention the individual tests used here.)
#'
#' @param x
#' @param y
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ordinal_tests <- function(x, y, ...) {

  c(
    wilcox = stats::wilcox.test(x[y==0],x[y==1])$p.value,
    fisher = stats::fisher.test(x,y,simulate.p.value=TRUE)$p.value,
    chi_sq_false = stats::chisq.test(x, y, correct=FALSE)$p.value,
    chi_sq_true = stats::chisq.test(x, y, correct=TRUE)$p.value,
    lrm = rms::lrm(x~y)$stats[5],
    coinasymp = coin::pvalue(coin::independence_test(x~y, ytrafo=coin::rank_trafo)),
    coinexact = coin::pvalue(coin::independence_test(x~y, ytrafo=coin::rank_trafo, distribution="exact"))
  )

}
