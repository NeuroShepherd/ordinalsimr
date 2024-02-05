
#' Parse Ratio Text
#'
#' This function parses text from ratios which are written in the format of 1-2 digit numbers separated by a colon and trailing with another 1-2 digit number. The text is processed into a numeric vector of length 2 containing the two numbers.
#'
#' @param text A string of in the form of e.g. 5:95 or 70:30
#'
#' @return Numeric vector of length 2
#' @export
#'
#' @import stringr
#'
#' @examples
#'
#' parse_ratio_text("70:30")
#'
parse_ratio_text <- function(text) {

  assert_that(str_detect(text, "[[:digit:]]{1,2}:[[:digit:]]{1,2}"),
              msg = "Incorrect ratio format.")

  pre_value <- as.numeric(str_extract(text, "^[[:digit:]]{1,2}"))
  post_value <- as.numeric(str_extract(text, "[[:digit:]]{1,2}$"))

  assert_that(pre_value + post_value == 100,
              msg = "Ratio does not sum to 100.")

  c(pre_value/100, post_value/100)

}







#' Calculate Hypothesis Test Parameters
#'
#' This function calculates the power, Type II error, and Type I error of tests given p-values
#'
#' @param df Data frame where each column is a vector of p-values from a statistical test
#' @param alpha Numeric significance level; defaults to 0.05
#' @param n Numeric value of sample size; repeated for convenience
#'
#' @return A data frame with columns for Type 1 error, Type 2 error, and power as well as rows for each test
#' @export
#'
calculate_power_t2error <- function(df, alpha = 0.05, power_confidence_int = 95, n = NA_real_) {

  z_score <- qnorm((100+power_confidence_int)/200)
  ci_label <- glue::glue("{power_confidence_int}% CI Interval")

  df %>%
    summarize(
      across(everything(),
             list(lower_power_se = ~mean(.x < alpha) - z_score*sqrt(mean(.x < alpha)*(1-mean(.x < alpha)))/length(.x),
                  upper_power_se = ~mean(.x < alpha) + z_score*sqrt(mean(.x < alpha)*(1-mean(.x < alpha)))/length(.x),
                  power = ~mean(.x < alpha),
                  t2_error = ~1-mean(.x < alpha)
             ),
             .names = "{.col}_{.fn}"
      )
    ) %>%
    # can't figure out how to format this correctly just off
    # pivot longer so need to separate and then pivot wider
    # again to get 3 separate columns for results
    # Solution(?): pivot_longer before summarize, group_by test,
    # and then calculate summary scores again?
    pivot_longer(cols = everything(),
                 names_to = "test",
                 values_to = "value") %>%
    separate(
      col = test,
      into = c("name", "statistic"),
      sep = "_",
      extra = "merge",
      remove = F
    ) %>%
    select(-test) %>%
    pivot_wider(
      names_from = statistic,
      names_glue = "{statistic}_{.value}",
      values_from = value
    ) %>%
    mutate("Sample Size" = n) %>%
    mutate(
      !!ci_label := glue::glue("[{round(lower_power_se_value, 4)}, {round(upper_power_se_value, 4)}]"),
      .after = power_value
    )


}




#' Plot Distribution
#'
#' This function takes a wide table of p-values (i.e. one column for each statistical test), converts it to long format, and creates a density plot of the p-values by each test.
#'
#' @param df data frame of p-values
#' @param alpha significance level
#' @param outlier_removal top x proportion of observations to filter out of table
#'
#' @return ggplot object
#' @importFrom rlang .data
#' @export
#'
plot_distribution_results <- function(df, alpha = 0.05, outlier_removal = 0.10) {

  df %>%
    pivot_longer(cols = everything(),names_to = "test_name") %>%
    slice_min(order_by = value, prop = outlier_removal ) %>%
    ggplot(aes(x = .data[["value"]], fill = .data[["test_name"]] )) +
    geom_density(alpha = 0.5, color = "black") +
    geom_vline(xintercept = alpha, linetype = "dashed", size = 2) +
    theme_bw()

}





