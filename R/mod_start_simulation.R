#' start_simulation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_start_simulation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run_button"), "Run Tests") %>%
      tagAppendAttributes(class = "btn btn-success")
  )
}

#' start_simulation Server Functions
#'
#' @noRd
mod_start_simulation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    run_simulation_button <- reactive({
      input$run_button
    })

    return(run_simulation_button)
  })
}

## To be copied in the UI
# mod_start_simulation_ui("start_simulation_1")

## To be copied in the server
# mod_start_simulation_server("start_simulation_1")
