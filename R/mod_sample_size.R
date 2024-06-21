#' sample_size UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets numericRangeInput
mod_sample_size_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericRangeInput(
      inputId = ns("sample_n"), label = "Sample Size Range",
      value = c(30, 80), step = 1
    )
  )
}

#' sample_size Server Functions
#'
#' @noRd
mod_sample_size_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    sample_n <- reactive({
      seq(round(input$sample_n[1]), round(input$sample_n[2]))
    })

    return(sample_n)
  })
}

## To be copied in the UI
# mod_sample_size_ui("sample_size_1")

## To be copied in the server
# mod_sample_size_server("sample_size_1")
