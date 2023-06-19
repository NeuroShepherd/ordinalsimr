#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  savior <- mod_data_entry_server("data_entry_1")


  # prob_data <- jsonlite::fromJSON( callModule( mod_data_entry_server, "data_entry_1" ("data_entry_1")$x$data))
  mod_test_pass_data_server("test_pass_data_1", prob_data = savior)

}
