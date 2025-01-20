#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # UI for data entry endpoints
  # Collect entered data
  add_row <- mod_row_add_server("row_add_1")
  delete_row <- mod_row_delete_server("row_delete_1")
  data_entered_probs <- mod_data_entry_server("data_entry_1",
    add_row = add_row,
    delete_row = delete_row
  )
  data_entered_iters <- mod_iterations_server("iterations_1")
  data_entered_sample_size <- mod_sample_size_server("sample_size_1")
  data_entered_tests <- mod_select_tests_server("select_tests_1")
  data_sample_probabilities <- mod_sample_probabilities_server("sample_probabilities_1")
  rng_selections <- mod_rng_option_server("rng_option_1")
  run_simulation_button <- mod_start_simulation_server("start_simulation_1")
  t1_error_toggle <- mod_type_one_error_server("type_one_error_1")


  # Pass collected data to stats calculations
  results_output <- mod_stats_calculations_server("stats_calculations_1",
    probability_data = data_entered_probs,
    sample_prob = data_sample_probabilities,
    iterations = data_entered_iters,
    sample_size = data_entered_sample_size,
    rng_info = rng_selections,
    included_tests = data_entered_tests,
    run_simulation_button = run_simulation_button,
    t1_error_toggle = t1_error_toggle
  )

  # Plot the distribution of values
  distributions_power_error <- mod_plot_distributions_server("plot_distributions_1",
    p_value_table = results_output,
    n = data_entered_sample_size
  )

  # format the data and plots into a list, and make this object available for
  # download as an .Rds/.Xlsx and make object available for the report
  formatted_data <- mod_save_data_server("save_data_1",
    input_data = results_output,
    processed_data = distributions_power_error,
    rng_info = rng_selections
  )

  mod_report_generator_server("report_generator_1",
    formatted_data = formatted_data,
    rng_info = rng_selections
  )

  session$onSessionEnded(function() {
    report_path <- system.file("report_template.html", package = "ordinalsimr")
    if (report_path != "") {file.remove(report_path)}
  })

}
