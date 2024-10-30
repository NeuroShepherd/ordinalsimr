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
      col_widths = c(5, 7, -2, 8, -2),
      row_heights = c(7, 9),
      navset_card_tab(
        full_screen = TRUE,
        title = "Inputs",
        nav_panel(
          "Core Inputs",
          layout_column_wrap(
            width = 1 / 2,
            heights_equal = "row",
            card(
              mod_iterations_ui("iterations_1"),
              mod_sample_size_ui("sample_size_1"),
              mod_sample_probabilities_ui("sample_probabilities_1")
            ),
            card(
              mod_select_tests_ui("select_tests_1")
            )
          )
        ),
        nav_panel(
          "Type I Erorr",
          mod_type_one_error_ui("type_one_error_1")
        ),
        nav_panel(
          "RNG Options",
          mod_rng_option_ui("rng_option_1")
        ),
        nav_panel(
          shiny::icon("circle-info"),
          markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)")
        )
      ),
      card(
        card_header("Data Entry", class = "bg-dark"),
        layout_sidebar(
          sidebar = sidebar(
            mod_row_add_ui("row_add_1"),
            mod_row_delete_ui("row_delete_1"),
            mod_start_simulation_ui("start_simulation_1")
          ),
          mod_data_entry_ui("data_entry_1")
        )
      ),
      navset_card_pill(
        title = "Simulation p-values",
        !!!mod_stats_calculations_ui("stats_calculations_1")
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
