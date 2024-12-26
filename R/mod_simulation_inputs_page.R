#' simulation_inputs_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_simulation_inputs_page_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Simulation Inputs",
    layout_columns(
      fill = TRUE,
      fillable = TRUE,
      col_widths = c(-1, 10, -1, -1, 10, -1),
      row_heights = c(9, 7),
      card(
        card_header("Data Entry", class = "bg-dark"),
        layout_sidebar(
          sidebar = sidebar(
            mod_row_add_ui("row_add_1"),
            mod_row_delete_ui("row_delete_1"),
            br(), br(), br(), br(),
            mod_start_simulation_ui("start_simulation_1")
          ),
          mod_data_entry_ui("data_entry_1")
        )
      ),
      navset_card_tab(
        full_screen = TRUE,
        title = "Inputs",
        nav_panel(
          "Core Inputs",
          card_title("Iterations, Samples, and Tests"),
          layout_column_wrap(
            width = 1 / 3,
            mod_iterations_ui("iterations_1"),
            mod_sample_size_ui("sample_size_1"),
            mod_sample_probabilities_ui("sample_probabilities_1")
          )
        ),
        nav_panel(
          "Select Tests",
          layout_column_wrap(
            width = 1,
            mod_select_tests_ui("select_tests_1")
          )
        ),
        nav_panel(
          "Type I Erorr",
          card_title("TI Error by Group"),
          mod_type_one_error_ui("type_one_error_1")
        ),
        nav_panel(
          "RNG Options",
          card_title("Advanced: Random Number Generator Adjustments"),
          markdown("These Random Number Generators are advanced options, and they use the default values employed by R as of version 4.4. Run `?RNGkind` in an R session to see the associated help file."),
          layout_column_wrap(
            width = 1 / 3,
            !!!mod_rng_option_ui("rng_option_1")
          )
        )
      )
    )
  )
}

#' simulation_inputs_page Server Functions
#'
#' @noRd
mod_simulation_inputs_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_simulation_inputs_page_ui("simulation_inputs_page_1")

## To be copied in the server
# mod_simulation_inputs_page_server("simulation_inputs_page_1")
