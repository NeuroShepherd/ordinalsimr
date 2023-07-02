
#' Ordinal outcome tests
#'
#'  (Description. Mention the individual tests used here.)
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
ordinal_tests <- function(x, y, sample_size, K) {

  c(
    # maxselA = (1-SNPmaxsel::maxsel.asymp.test(x1=x,y=y,type="ord")$value),
    # maxselE = exactmaxsel2::maxsel.test(x=x,y=y,statistic="chi2")@maxsel_p_value,
      # ^ packages not currently on CRAN--future uncertain
    wilcox = stats::wilcox.test(x[y==0],x[y==1])$p.value,
    fisher = stats::fisher.test(x,y,simulate.p.value=TRUE)$p.value,
    chi_sq_false = stats::chisq.test(x, y, correct=FALSE)$p.value,
    chi_sq_true = stats::chisq.test(x, y, correct=TRUE)$p.value,
    # prop.trend.test(my.tab[,2],rowSums(my.tab))$p.value,
    # ^doesn't like when there are 0 obs in a category, I think
    lrm = rms::lrm(x~y)$stats[5],
    coinasymp = coin::pvalue(coin::independence_test(x~y, ytrafo=coin::rank_trafo)),
    coinexact = coin::pvalue(coin::independence_test(x~y, ytrafo=coin::rank_trafo, distribution="exact"))
  )

}
