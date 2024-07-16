#' Parse Ratio Text
#'
#' This function parses text from ratios which are written in the format of 1-2 digit numbers separated by a colon and trailing with another 1-2 digit number. The text is processed into a numeric vector of length 2 containing the two numbers.
#'
#' @param text A string of in the form of e.g. 5:95 or 70:30
#'
#' @return Numeric vector of length 2
#' @export
#'
#'
#' @examples
#'
#' parse_ratio_text("70:30")
#'
parse_ratio_text <- function(text) {
  assert_that(grepl("[[:digit:]]{1,2}:[[:digit:]]{1,2}", text),
    msg = "Incorrect ratio format."
  )

  pre_value <- as.numeric(regmatches(text, regexpr("^[[:digit:]]{1,2}", text)))
  post_value <- as.numeric(regmatches(text, regexpr("[[:digit:]]{1,2}$", text)))

  assert_that(pre_value + post_value == 100,
    msg = "Ratio does not sum to 100."
  )

  c(pre_value / 100, post_value / 100)
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
#' @export
#'
calculate_power_t2error <- function(df, alpha = 0.05, power_confidence_int = 95, n = NA_real_) {
  ci_power_label <- paste0("Power ", power_confidence_int, "% CI")
  ci_t2error_label <- paste0("TII Error ", power_confidence_int, "% CI")

  df %>%
    group_by(.data[["sample_size"]]) %>%
    dplyr::group_modify(
      ~ {
        purrr::map(.x, ~ {
          binom_power <- binom.test(sum(.x < alpha), length(.x), conf.level = power_confidence_int / 100)
          tibble(
            lower_power_bound = binom_power$conf.int[[1]],
            upper_power_bound = binom_power$conf.int[[2]],
            power = binom_power$estimate,
            !!ci_power_label := paste0("[",round(lower_power_bound, 4), ", ", round(upper_power_bound, 4), "]"),
            lower_t2error_bound = 1 - upper_power_bound,
            upper_t2error_bound = 1 - lower_power_bound,
            t2_error = 1 - binom_power$estimate,
            !!ci_t2error_label := paste0("[",round(lower_t2error_bound, 4), ", ", round(upper_t2error_bound, 4), "]")
          )
        }) %>%
          purrr::list_rbind(names_to = "test")
      }
    ) %>%
    rename("Sample Size" = "sample_size")
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
#' @export
#'
calculate_t1_error <- function(df, alpha = 0.05, t1_error_confidence_int = 95, n = NA_real_) {
  ci_label <- paste0(t1_error_confidence_int, "% CI")

  df %>%
    group_by(.data[["sample_size"]]) %>%
    dplyr::group_modify(
      ~ {
        purrr::map(.x, ~ {

          tryCatch({

            binom_results <- binom.test(sum(.x < alpha, na.rm = T),
                                        sum(!is.na(.x)),
                                        conf.level = t1_error_confidence_int / 100)

              tibble(
                lower_t1_bound = binom_results$conf.int[[1]],
                upper_t1_bound = binom_results$conf.int[[2]],
                t1_error = binom_results$estimate,
                !!ci_label := paste0("[",round(lower_t1_bound, 4), ", ", round(upper_t1_bound, 4), "]")
              )

              },
            error = function(e) {
              tibble(
                lower_t1_bound = NA_real_,
                upper_t1_bound = NA_real_,
                t1_error = NA_real_,
                !!ci_label := NA_character_
              )
            })

        }) %>%
          purrr::list_rbind(names_to = "test")
      }
    ) %>%
    rename("Sample Size" = "sample_size")
}



#' Plot Distribution
#'
#' This function takes a wide table of p-values (i.e. one column for each statistical test), converts it to long format, and creates a density plot of the p-values by each test.
#'
#' @param df data frame where each column is a set of p-values for a different statistical test
#' @param alpha numeric. significance level
#' @param outlier_removal numeric. set x-axis scale maximum by proportion
#'
#' @return ggplot object
#' @export
#'
plot_distribution_results <- function(df, alpha = 0.05, outlier_removal = 0.10) {
  df %>%
    pivot_longer(cols = -.data$sample_size, names_to = "test_name") %>%
    mutate(test_name = stats::reorder(.data[["test_name"]], .data[["value"]], decreasing = TRUE)) %>%
    group_by(.data$sample_size, .data$test_name) %>%
    summarise(value = mean(.data$value)) %>%
    {
      ggplot(., aes(x = .data[["sample_size"]], y = .data[["value"]], color = .data[["test_name"]])) +
        geom_line() +
        geom_hline(yintercept = alpha, linetype = "dashed", size = 2) +
        ggtitle("Plot of p-values") +
        labs(x = "Sample Size", y = "p-value", color = "Statistical Test") +
        guides(fill = "none") +
        theme_bw() +
        theme(
          axis.text = element_text(face = "bold", size = 14),
          axis.title = element_text(face = "bold", size = 18),
          plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
        )
    }
}






#' Format Simulation Data
#'
#' Soft wrapper for `dplyr::bind_rows()`: formats the list of data from running simulations into a list of tibbles; one tibble for the p-values for each run of each model, and one tibble with run metadata. List objects are named `p_values` and `run_info`. The tibble in `run_info` further contains two nested tibbles.
#'
#' @param input a named list structured as the output from `run_simulations()`
#'
#' @return a named list with a p_value tibble and a run_info nested tibble
#' @export
#'
format_simulation_data <- function(input) {
  input %>%
    bind_rows()
}



#' Plot Test Power
#'
#' @param df a dataframe with p-values and a sample_size column
#'
#' @return ggplot object
#' @export
#'
plot_power <- function(df) {
  df %>%
    ggplot(aes(
      x = .data[["Sample Size"]], y = .data[["power"]],
      ymin = .data[["lower_power_bound"]], ymax = .data[["upper_power_bound"]],
      color = .data[["test"]], fill = .data[["test"]]
    )) +
    geom_line() +
    theme_bw() +
    theme(
      axis.text = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold", size = 18),
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
    )
}
