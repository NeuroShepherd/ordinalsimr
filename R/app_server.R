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

  # Pass collected data to stats calculations
  mod_stats_calculations_server("stats_calculations_1",
                                probability_data = data_entered_probs,
                                iterations = NULL,
                                sample_size = NULL)

  # PLACEHOLDER: pass results to output options such as .Rdata/.csv
  # and any other enhanced functionality

}
