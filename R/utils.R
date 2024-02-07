
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
#' This function calculates the power, Type II error, and Type I error of tests given p-values. Z-score calculated using `qnorm()`, and assumed to be two-sided.
#'
#' @param df Data frame where each column is a vector of p-values from a statistical test
#' @param alpha Numeric significance level; defaults to 0.05
#' @param power_confidence_int confidence interval
#' @param n Numeric value of sample size; repeated for convenience
#'
#' @return A data frame with columns for Type 1 error, Type 2 error, and power as well as rows for each test
#' @export
#'
calculate_power_t2error <- function(df, alpha = 0.05, power_confidence_int = 95, n = NA_real_) {

  z_score <- qnorm((100+power_confidence_int)/200)
  ci_label <- glue::glue("{power_confidence_int}% CI Interval")

  df %>%
    pivot_longer(cols = everything(), names_to = "test", values_to = "value") %>%
    group_by(test) %>%
    summarize(lower_power_bound = mean(value < alpha) - z_score*sqrt(mean(value < alpha)*(1-mean(value < alpha)))/length(value),
              upper_power_bound = mean(value < alpha) + z_score*sqrt(mean(value < alpha)*(1-mean(value < alpha)))/length(value),
             power = mean(value < alpha),
             !!ci_label := glue::glue("[{round(lower_power_bound, 4)}, {round(upper_power_bound, 4)}]"),
             t2_error = 1-power
    ) %>%
    mutate("Sample Size" = n)

}




#' Calculate Type 1 Error
#'
#' Calculate Type 1 error for a distribution, and the confidence interval around this estimate. Z-score calculated using `qnorm()`, and assumed to be two-sided.
#'
#' @param df data frame
#' @param alpha significance level
#' @param t1_error_confidence_int confidence interval
#' @param n optional numeric input of
#'
#' @return data frame
#' @export
#'
calculate_t1_error <- function(df, alpha = 0.05, t1_error_confidence_int = 95, n = NA_real_) {

  z_score <- qnorm((100+t1_error_confidence_int)/200)
  ci_label <- glue::glue("{t1_error_confidence_int}% CI Interval")

  df %>%
    pivot_longer(cols = everything(), names_to = "test", values_to = "value") %>%
    group_by(test) %>%
    summarize(lower_t1_bound = mean(value < alpha) - z_score*sqrt(mean(value < alpha)*(1-mean(value < alpha)))/length(value),
              upper_t1_bound = mean(value < alpha) + z_score*sqrt(mean(value < alpha)*(1-mean(value < alpha)))/length(value),
              t1_error = mean(value < alpha),
              !!ci_label := glue::glue("[{round(lower_t1_bound,4)}, {round(upper_t1_bound,4)}]")
    ) %>%
    mutate("Sample Size" = n)

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
    ggplot(aes(x = .data[["value"]], color = .data[["test_name"]], fill = .data[["test_name"]] )) +
    geom_density(alpha = 0.1, size = 2.5) +
    geom_vline(xintercept = alpha, linetype = "dashed", size = 2) +
    title("Density Plot of p-values") +
    labs(x = "p-value", y = "Density") +
    theme_bw() +
    theme(
      legend.key.size = unit(1, 'cm'),
      legend.text = element_text(size = 14),
      legend.title = element_text(face = "bold", size = 16),
      axis.text = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold", size = 18)
      )

}





