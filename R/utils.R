
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
#' This function calculates the power, Type II error, and Type I error of tests given p-values. Power, Type II error, and confidence intervals calculated using `stats::binom.test()` which implements the Newcombe method.
#'
#' @param df Data frame where each column is a vector of p-values from a statistical test
#' @param alpha Numeric significance level; defaults to 0.05
#' @param power_confidence_int confidence interval
#' @param n Numeric value of sample size; repeated for convenience
#'
#' @return A data frame with columns for Type 1 error, Type 2 error, and power as well as rows for each test
#' @importFrom rlang :=
#' @export
#'
calculate_power_t2error <- function(df, alpha = 0.05, power_confidence_int = 95, n = NA_real_) {

  ci_power_label <- glue::glue("Power {power_confidence_int}% CI Interval")
  ci_t2error_label <- glue::glue("TII Error {power_confidence_int}% CI Interval")

  df %>%
    purrr::map(
      ~{
        binom_power <- binom.test(sum(.x < alpha), length(.x), conf.level = power_confidence_int/100)
        tibble(
          lower_power_bound = binom_power$conf.int[[1]],
          upper_power_bound = binom_power$conf.int[[2]],
          power = binom_power$estimate,
          !!ci_power_label := glue::glue("[{round(lower_power_bound, 4)}, {round(upper_power_bound, 4)}]"),
          lower_t2error_bound = 1-upper_power_bound,
          upper_t2error_bound = 1-lower_power_bound,
          t2_error = 1 - binom_power$estimate,
          !!ci_t2error_label := glue::glue("[{round(lower_t2error_bound, 4)}, {round(upper_t2error_bound, 4)}]"))
      }
    ) %>%
    purrr::list_rbind(names_to = "test") %>%
    mutate("Sample Size" = n)

}




#' Calculate Type 1 Error
#'
#' Calculate Type 1 error for a distribution, and the confidence interval around this estimate. Type I error and confidence intervals calculated using `stats::binom.test()` which implements the Newcombe method.
#'
#' @param df data frame
#' @param alpha significance level
#' @param t1_error_confidence_int confidence interval
#' @param n optional numeric input of
#'
#' @return data frame
#' @importFrom rlang :=
#' @export
#'
calculate_t1_error <- function(df, alpha = 0.05, t1_error_confidence_int = 95, n = NA_real_) {

  ci_label <- glue::glue("{t1_error_confidence_int}% CI Interval")

  df %>%
    purrr::map(
      ~{
        binom_results <- binom.test(sum(.x < alpha), length(.x), conf.level = t1_error_confidence_int/100)
        tibble(
          lower_t1_bound = binom_results$conf.int[[1]],
          upper_t1_bound = binom_results$conf.int[[2]],
          t1_error = binom_results$estimate,
          !!ci_label := glue::glue("[{round(lower_t1_bound,4)}, {round(upper_t1_bound,4)}]"))
      }
      ) %>%
    purrr::list_rbind(names_to = "test") %>%
    mutate("Sample Size" = n)


}



#' Plot Distribution
#'
#' This function takes a wide table of p-values (i.e. one column for each statistical test), converts it to long format, and creates a density plot of the p-values by each test.
#'
#' @param df data frame of p-values
#' @param alpha numeric. significance level
#' @param outlier_removal numeric. set x-axis scale maximum by proportion
#'
#' @return ggplot object
#' @importFrom rlang .data
#' @importFrom ggridges geom_density_ridges
#' @export
#'
plot_distribution_results <- function(df, alpha = 0.05, outlier_removal = 0.10) {

  df %>%
    pivot_longer(cols = everything(),names_to = "test_name") %>%
    {
    xaxis_lim <- (outlier_removal)*max(pull(.,"value"))
    ggplot(., aes(x = .data[["value"]], y = .data[["test_name"]], color = .data[["test_name"]], fill = .data[["test_name"]] )) +
    ggridges::geom_density_ridges(alpha = 0.2, panel_scaling = TRUE) +
    geom_vline(xintercept = alpha, linetype = "dashed", size = 2) +
    scale_x_continuous(limits = c(0, xaxis_lim)) +
    ggtitle("Density Plot of p-values") +
    labs(x = "p-value", y = "Density", color = "Statistical Test") +
    guides(fill = "none") +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold", size = 18),
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
      )
    }
}






#' Format Simulation Data
#'
#' Formats the list of data from running simulations into a list of tibbles; one tibble for the p-values for each run of each model, and one tibble with run metadata. List objects are named `p_values` and `run_info`. The tibble in `run_info` further contains two nested tibbles.
#'
#' @param input a named list structured as the output from `run_simulations()`
#'
#' @return a named list with a p_value tibble and a run_info nested tibble
#' @importFrom magrittr extract2
#' @export
#'
format_simulation_data <- function(input) {

  run_info <- input %>%
    magrittr::extract2("initial_groups") %>%
    purrr::map_df(~tibble(
      total_sample_size = .x[["sample_size"]],
      group1_count = .x[["n_null"]],
      group2_count = .x[["n_intervene"]],
      outcome_vars_count = .x[["K"]],
      assigned_groups = list(tibble(.x[["y"]])),
      assigned_values = list(tibble(.x[["x"]]))
    ))

  return(
    list(p_values = tibble(as.data.frame(input[["p_values"]])), run_info = run_info)
    )

}


