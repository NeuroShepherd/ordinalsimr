#' distributions_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_distributions_page_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Results and Plots",
    layout_columns(
      col_widths = c(12, 4, 8),
      fillable = TRUE,
      navset_card_pill(
        full_screen = TRUE,
        title = "Plot Output",
        nav_panel(
          title = "Power",
          mod_plot_distributions_ui("plot_distributions_1")[["output_plots"]][["power_plot"]]
        ),
        nav_panel(
          title = "p-values",
          mod_plot_distributions_ui("plot_distributions_1")[["output_plots"]][["p_val_plot"]]
        )
      ),
      navset_card_tab(
        full_screen = TRUE,
        title = "Inputs",
        nav_panel(
          "Core Inputs",
          layout_column_wrap(
            width = 1 / 2,
            heights_equal = "row",
            card(
              markdown("Confidence Intervals"),
              !!!mod_plot_distributions_ui("plot_distributions_1")[["ci_inputs"]]
            ),
            card(
              markdown("p-value Threshold"),
              mod_plot_distributions_ui("plot_distributions_1")[["p_val_input"]],
              markdown("Power Threshold"),
              mod_plot_distributions_ui("plot_distributions_1")[["power_val_input"]]
            )
          )
        ),
        nav_panel(
          "Filter Outliers",
          mod_plot_distributions_ui("plot_distributions_1")[["outlier_input"]]
        )
      ),
      navset_card_pill(
        title = "Results",
        !!!mod_plot_distributions_ui("plot_distributions_1")[["output_data"]]
      )
    )
  )
}

#' distributions_page Server Functions
#'
#' @noRd
mod_distributions_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_distributions_page_ui("distributions_page_1")

## To be copied in the server
# mod_distributions_page_server("distributions_page_1")
