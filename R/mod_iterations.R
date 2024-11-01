#' iterations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_iterations_ui <- function(id) {
  ns <- NS(id)

  default_iterations <- getOption("ordinalsimr.default_iterations", default = 1000)
  tagList(
    shiny::numericInput(ns("iterations"), "Number of Iterations",
      value = default_iterations, min = 1, max = Inf
    )
  )
}

#' iterations Server Functions
#'
#' @noRd
mod_iterations_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    iterations <- reactive({
      input$iterations
    })

    return(iterations)
  })
}

## To be copied in the UI
# mod_iterations_ui("iterations_1")

## To be copied in the server
# mod_iterations_server("iterations_1")
