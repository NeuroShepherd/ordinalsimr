#' Simulation Data for Two Groups
#'
#' Simulated p-values and metadata for a two group comparison. See `simulation_data_two_groups_formatted` for a tidy, recommended version of this same data. Useful for Type II error and power calculations.
#'
#' @format ## `simulation_data_two_groups`
#' A list
#' \describe{
#'   \item{p_values}{A data frame of p-values from each run of each test}
#'   \item{initial_groups}{A nested list with information for each simulation run}
#' }

"simulation_data_two_groups"

#' Formatted Simulation Data for Two Groups
#'
#' Simulated p-values and metadata for a two group comparison in a tidy format. Useful for Type II error and power calculations.
#'
#' @format ## `simulation_data_two_groups_formatted`
#' A list
#' \describe{
#'   \item{p_values}{A data frame of p-values from each run of each test}
#'   \item{initial_groups}{A nested list with information for each simulation run}
#' }

"simulation_data_two_groups_formatted"

#' Simulation Data for One Group
#'
#' Simulated p-values and metadata for a two group comparison. See `simulation_data_one_group_formatted` for a tidy, recommended version of this same data. Useful for Type I error calculations.
#'
#' @format ## `simulation_data_one_group`
#' A list
#' \describe{
#'   \item{p_values}{A data frame of p-values from each run of each test}
#'   \item{initial_groups}{A nested list with information for each simulation run}
#' }

"simulation_data_one_group"

#' Formatted Simulation Data for One Group
#'
#' Simulated p-values and metadata for a two group comparison in a tidy format. Useful for Type I error calculations.
#'
#' @format ## `simulation_data_one_group_formatted`
#' A list
#' \describe{
#'   \item{p_values}{A data frame of p-values from each run of each test}
#'   \item{initial_groups}{A nested list with information for each simulation run}
#' }

"simulation_data_one_group_formatted"
