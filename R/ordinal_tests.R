#' Ordinal outcome tests
#'
#' A metafunction that runs the statistical tests listed below, and returns the p-values as a named vector.
#'
#' \itemize{
#'  \item{stats::wilcox.test()}
#'  \item{stats::fisher.test(simulate.p.value = TRUE)}
#'  \item{stats::chisq.test(correct = FALSE)}
#'  \item{stats::chisq.test(correct = TRUE)}
#'  \item{rms::lrm()}
#'  \item{coin::independence_test(ytrafo = coin::rank_trafo)}
#'  }
#' @param x Group one
#' @param y Group two
#' @param included a character vector of the tests to be included. Default is "all"
#' @param ... Placeholder for additional arguments to functions
#'
#' @return A named matrix of probabilities for each test
#'
#' The function is designed to run all 6 tests by default. If you want to run only a subset of the tests, you can specify them in the `included` argument. The following values are possible:
#'
#' \itemize{
#' \item{"Wilcoxon"}
#' \item{"Fisher"}
#' \item{"Chi Squared (No Correction)"}
#' \item{"Chi Squared (Correction)"}
#' \item{"Prop. Odds"}
#' \item{"Coin Indep. Test"}
#' }
#'
#' This option is primarily for use in the Shiny application.
#'
#'
#' @export
#'
ordinal_tests <- function(x, y, included = "all", ...) {
  assertthat::assert_that(
    length(x) == length(y),
    msg = "x and y must have the same length"
  )

  assertthat::assert_that(
    is.character(included) | is.character(included),
    msg = "included must be a character vector"
  )

  assertthat::assert_that(
    all(included %in% c(
      "all", "Wilcoxon", "Fisher", "Chi Squared (No Correction)",
      "Chi Squared (Correction)", "Prop. Odds", "Coin Indep. Test"
    )),
    msg = "included must be a subset of the possible tests"
  )


  if ("all" %in% included) {
    included <- c(
      "Wilcoxon", "Fisher", "Chi Squared (No Correction)",
      "Chi Squared (Correction)", "Prop. Odds", "Coin Indep. Test"
    )
  }


  if ("Wilcoxon" %in% included) {
    wilcoxon <- tryCatch(
      {
        suppressWarnings(
          stats::wilcox.test(x[y == 0], x[y == 1])[["p.value"]]
        )
      },
      error = function(e) {
        NA_real_
      }
    )
  } else {
    wilcoxon <- NULL
  }

  if ("Fisher" %in% included) {
    fisher <- tryCatch(
      {
        stats::fisher.test(x, y, simulate.p.value = TRUE)[["p.value"]]
      },
      error = function(e) {
        NA_real_
      }
    )
  } else {
    fisher <- NULL
  }

  if ("Chi Squared (No Correction)" %in% included) {
    chi_sq_no_correction <- tryCatch(
      {
        suppressWarnings(
          stats::chisq.test(x, y, correct = FALSE)[["p.value"]]
        )
      },
      error = function(e) {
        NA_real_
      }
    )
  } else {
    chi_sq_no_correction <- NULL
  }

  if ("Chi Squared (Correction)" %in% included) {
    chi_sq_correction <- tryCatch(
      {
        suppressWarnings(
          stats::chisq.test(x, y, correct = TRUE)[["p.value"]]
        )
      },
      error = function(e) {
        NA_real_
      }
    )
  } else {
    chi_sq_correction <- NULL
  }

  if ("Prop. Odds" %in% included) {
    prop_odds <- tryCatch(
      {
        rms::lrm(x ~ y)$stats[["P"]]
      },
      error = function(e) {
        NA_real_
      }
    )
  } else {
    prop_odds <- NULL
  }

  if ("Coin Indep. Test" %in% included) {
    coin_indep_test <- tryCatch(
      {
        coin::pvalue(coin::independence_test(x ~ y, ytrafo = coin::rank_trafo))
      },
      error = function(e) {
        NA_real_
      }
    )
  } else {
    coin_indep_test <- NULL
  }

  output <- matrix(c(
    Wilcoxon = wilcoxon,
    Fisher = fisher,
    "Chi Squared\n(No Correction)" = chi_sq_no_correction,
    "Chi Squared\n(Correction)" = chi_sq_correction,
    "Prop. Odds" = prop_odds,
    "Coin Indep. Test" = coin_indep_test
  ),
  ncol = length(included))

  colnames(output) <- included

  return(output)
}
