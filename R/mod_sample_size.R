#' sample_size UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sample_size_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::numericInput(ns("sample_n"), "Sample Size", 30, min = 1, max = Inf)
  )
}

#' sample_size Server Functions
#'
#' @noRd
mod_sample_size_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    sample_n <- reactive({input$sample_n})

    return(sample_n)

  })
}

## To be copied in the UI
# mod_sample_size_ui("sample_size_1")

## To be copied in the server
# mod_sample_size_server("sample_size_1")
