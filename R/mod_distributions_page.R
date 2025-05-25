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
      fillable = TRUE,
      navset_card_tab(
        full_screen = TRUE,
        title = "Plots and Tables",
        sidebar = sidebar(
          title = "Inputs",
          h6("Testing"),
          mod_plot_distributions_ui("plot_distributions_1")[["p_val_input"]],
          mod_plot_distributions_ui("plot_distributions_1")[["power_val_input"]],
          h6("Confidence Intervals"),
          !!!mod_plot_distributions_ui("plot_distributions_1")[["ci_inputs"]]
          # h6("Outlier Removal (Plot Only)"),
          # mod_plot_distributions_ui("plot_distributions_1")[["outlier_input"]]
        ),
        nav_panel(
          title = "Power",
          tags$head(
            tags$style(HTML("
              .plot-container {
                position: relative;
              }
              .overlay-checkbox {
                position: absolute;
                top: 0px;
                right: -150px;
                z-index: 10;
                background: rgba(255, 255, 255, 0.8);
                padding: 5px 10px;
                border-radius: 4px;
              }
            "))
          ),
          div(class = "plot-container",
              div(class = "overlay-checkbox", checkboxInput(ns("show_confidence_interval"), "Show CI", value = TRUE)),
              mod_plot_distributions_ui("plot_distributions_1")[["output_plots"]][["power_plot"]]
          ),
        ),
        nav_panel(
          title = "p-values",
          mod_plot_distributions_ui("plot_distributions_1")[["output_plots"]][["p_val_plot"]]
        ),
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

    return(reactive({input$show_confidence_interval}))

  })
}

## To be copied in the UI
# mod_distributions_page_ui("distributions_page_1")

## To be copied in the server
# mod_distributions_page_server("distributions_page_1")
