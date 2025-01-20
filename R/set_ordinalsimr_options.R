#' Set Option Helper
#'
#' @param option_name name of the ordinalsimr option to set
#' @param value value to set the option to
#' @param additional_msg additional message to print after setting the option
#'
#' @return invisible
#' @keywords internal
#'
.set_options_helper <- function(option_name, value, additional_msg = NULL) {
  tryCatch(
    expr = {
      options(rlang::list2({{ option_name }} := value))

      if (is.null(value)) {
        NULL
      }

      message("Setting ", option_name, " to ", as.character(value))
      if (!is.null(additional_msg)) {
        message(additional_msg)
      }
    },
    error = function(e) {
      invisible()
    }
  )
}




#' Set ordinalsimr Shiny App Default Values
#'
#' @param default_iterations number of iterations to run
#' @param default_size_min number for the small end of the sample size range
#' @param default_size_max number for the large end of the sample size range
#' @param default_ratio text ratio of the number of levels in the two groups, format of "50:50"
#' @param default_distributions data frame of the distributions of the levels in the two groups
#' @param default_entry_rows number of rows to initialize the (empty) data frame with
#'
#' @return invisible
#' @export
#'
#' @examples
#'
#' # Set the default values for the ordinalsimr Shiny app
#'
#' set_ordinalsimr_options(
#'   default_iterations = 1000,
#'   default_size_min = 10,
#'   default_size_max = 100,
#'   default_ratio = "50:50",
#'   default_distributions = data.frame(c(0.4, 0.3, 0.3), c(0.8, 0.1, 0.1))
#' )
#'
#' # Values can be either overwritten or unset by setting them to NULL. The Shiny
#' # app still has backup values if these options are not set. Not all arguments
#' # need to be provided
#'
#' set_ordinalsimr_options(
#'   default_iterations = 500, # Ex: update argument
#'   default_size_min = NULL, # Ex: unset argument
#'   default_size_max = NULL, # Ex: unset argument
#'   # default_ratio = NULL, # Ex: arg not provided (by commenting out)
#'   default_distributions = NULL
#' )
#'
set_ordinalsimr_options <- function(
    default_iterations,
    default_size_min,
    default_size_max,
    default_ratio,
    default_distributions,
    default_entry_rows) {
  .set_options_helper("ordinalsimr.default_iterations", default_iterations)
  .set_options_helper("ordinalsimr.default_size_min", default_size_min)
  .set_options_helper("ordinalsimr.default_size_max", default_size_max)
  .set_options_helper("ordinalsimr.default_ratio", default_ratio)
  .set_options_helper("ordinalsimr.default_distributions", default_distributions)
  .set_options_helper("ordinalsimr.default_entry_rows", default_entry_rows, additional_msg = "The ordinalsimr.default_distributions option takes precedence over the ordinalsimr.default_entry_rows option if it has been set.")
}



#' Get ordinalsimr options
#'
#' Returns all of the ordinalsimr options to the console.
#'
#' @return list of ordinalsimr options
#' @export
#'
#' @examples
#' get_ordinalsimr_options()
get_ordinalsimr_options <- function() {
  c(
    "ordinalsimr.default_iterations",
    "ordinalsimr.default_size_min",
    "ordinalsimr.default_size_max",
    "ordinalsimr.default_ratio",
    "ordinalsimr.default_distributions",
    "ordinalsimr.default_entry_rows"
  ) %>%
    stats::setNames(., .) %>%
    lapply(function(x) getOption(x))
}
