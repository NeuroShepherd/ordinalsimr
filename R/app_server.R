#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # UI for data entry endpoints
  # Collect entered data
  data_entered_probs <- mod_data_entry_server("data_entry_1")
  data_entered_iters <- mod_iterations_server("iterations_1")
  data_entered_sample_size <- mod_sample_size_server("sample_size_1")
  data_sample_probabilities <- mod_sample_probabilities_server("sample_probabilities_1")
  rng_selections <- mod_rng_option_server("rng_option_1")


  # Pass collected data to stats calculations
  results_output <- mod_stats_calculations_server("stats_calculations_1",
                                probability_data = data_entered_probs,
                                sample_prob = data_sample_probabilities,
                                iterations = data_entered_iters,
                                sample_size = data_entered_sample_size,
                                rng_info = rng_selections)

  # Plot the distribution of values
  distributions_power_error <- mod_plot_distributions_server("plot_distributions_1",
                                p_value_table = results_output,
                                n = data_entered_sample_size)

  # PLACEHOLDER: pass results to output options such as .Rdata/.csv
  # and any other enhanced functionality
  mod_save_data_server("save_data_1",
                       input_data = results_output,
                       processed_data = distributions_power_error,
                       rng_info = rng_selections)

}
