#' test_pass_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_test_pass_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::dataTableOutput(ns("hello_there"))
  )
}

#' test_pass_data Server Functions
#'
#' @noRd
mod_test_pass_data_server <- function(id, prob_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$hello_there <- shiny::renderDataTable(prob_data())
  })
}

## To be copied in the UI
# mod_test_pass_data_ui("test_pass_data_1")

## To be copied in the server
# mod_test_pass_data_server("test_pass_data_1")
