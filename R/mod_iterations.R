#' iterations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_iterations_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::numericInput(ns("iterations"), "Number of Iterations", 1, min = 1, max = Inf)
  )
}

#' iterations Server Functions
#'
#' @noRd
mod_iterations_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    iterations <- reactive({input$iterations})

    return(iterations)

  })
}

## To be copied in the UI
# mod_iterations_ui("iterations_1")

## To be copied in the server
# mod_iterations_server("iterations_1")
