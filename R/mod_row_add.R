#' row_add UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_row_add_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("add_row"), "Add Row")
  )
}

#' row_add Server Functions
#'
#' @noRd
mod_row_add_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    add_row_button <- reactive({
      input$add_row
    })

    return(add_row_button)
  })
}

## To be copied in the UI
# mod_row_add_ui("row_add_1")

## To be copied in the server
# mod_row_add_server("row_add_1")
