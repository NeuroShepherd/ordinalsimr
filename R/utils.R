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
        lapply(., function(x) {
          binom_power <- binom.test(sum(x < alpha), length(x), conf.level = power_confidence_int / 100)
          tibble(
            lower_power_bound = binom_power$conf.int[[1]],
            upper_power_bound = binom_power$conf.int[[2]],
            power = binom_power$estimate,
            !!ci_power_label := paste0("[", round(lower_power_bound, 3), ", ", round(upper_power_bound, 3), "]"),
            lower_t2error_bound = 1 - upper_power_bound,
            upper_t2error_bound = 1 - lower_power_bound,
            t2_error = 1 - binom_power$estimate,
            !!ci_t2error_label := paste0("[", round(lower_t2error_bound, 3), ", ", round(upper_t2error_bound, 3), "]")
          )
        }) %>%
          bind_rows(.id = "test")
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
        lapply(., function(x) {
          tryCatch(
            {
              binom_results <- binom.test(sum(x < alpha, na.rm = T),
                sum(!is.na(x)),
                conf.level = t1_error_confidence_int / 100
              )

              tibble(
                lower_t1_bound = binom_results$conf.int[[1]],
                upper_t1_bound = binom_results$conf.int[[2]],
                t1_error = binom_results$estimate,
                !!ci_label := paste0("[", round(lower_t1_bound, 3), ", ", round(upper_t1_bound, 3), "]")
              )
            },
            error = function(e) { # nocov start
              tibble(
                lower_t1_bound = NA_real_,
                upper_t1_bound = NA_real_,
                t1_error = NA_real_,
                !!ci_label := NA_character_
              )
            } # nocov end
          )
        }) %>%
          bind_rows(.id = "test")
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
  levels <- df %>%
    pivot_longer(cols = -all_of("sample_size"), names_to = "test_name") %>%
    group_by(.data[["test_name"]]) %>%
    summarize(
      mean = mean(.data[["value"]], na.rm = T)
    ) %>%
    arrange(.data[["mean"]]) %>%
    pull(.data[["test_name"]])

  df %>%
    pivot_longer(cols = -all_of("sample_size"), names_to = "test_name") %>%
    mutate(test_name = factor(.data$test_name, levels = levels)) %>%
    group_by(.data$sample_size, .data$test_name) %>%
    summarise(value = mean(.data$value)) %>%
    {
      ggplot(., aes(
        x = .data[["sample_size"]],
        y = .data[["value"]],
        color = .data[["test_name"]]
      )) +
        geom_line(linewidth = 2) +
        geom_hline(yintercept = alpha, linetype = "dashed", linewidth = 1.5) +
        expand_limits(y = 0) +
        ggtitle("Mean p-value") +
        labs(x = "Sample Size", y = "p-value", color = "Statistical Test") +
        guides(fill = "none", linetype = "none") +
        theme_bw() +
        theme(
          axis.text = element_text(face = "bold", size = 14),
          axis.title = element_text(face = "bold", size = 18),
          plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16, face = "bold")
        )
    }
}



#' Plot Test Power
#'
#' @param df a dataframe with p-values and a sample_size column
#' @param power_threshold numeric. desired power threshold
#'
#' @return ggplot object
#' @export
#'
plot_power <- function(df, power_threshold = 0.80) {
  levels <- df %>%
    group_by(.data[["test"]]) %>%
    summarize(
      mean = mean(.data[["power"]], na.rm = T)
    ) %>%
    arrange(desc(.data[["mean"]])) %>%
    pull(.data[["test"]])

  df %>%
    mutate(test = factor(.data[["test"]], levels = levels)) %>%
    ggplot(aes(
      x = .data[["Sample Size"]], y = .data[["power"]],
      ymin = .data[["lower_power_bound"]], ymax = .data[["upper_power_bound"]],
      color = .data[["test"]], fill = .data[["test"]]
    )) +
    geom_line(linewidth = 2) +
    geom_hline(yintercept = power_threshold, linetype = "dashed", linewidth = 1.5) +
    expand_limits(y = c(0, 1)) +
    ggtitle("Estimated Power") +
    labs(x = "Sample Size", y = "Power (1-\U03B2)", color = "Statistical Test") +
    guides(fill = "none", linetype = "none") +
    theme_bw() +
    theme(
      axis.text = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold", size = 18),
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold")
    )
}
