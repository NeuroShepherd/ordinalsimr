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


  # Pass collected data to stats calculations
  results_output <- mod_stats_calculations_server("stats_calculations_1",
                                probability_data = data_entered_probs,
                                iterations = data_entered_iters,
                                sample_size = data_entered_sample_size)

  # PLACEHOLDER: pass results to output options such as .Rdata/.csv
  # and any other enhanced functionality
  mod_save_data_server("save_data_1", data = results_output()$p_values)

}
