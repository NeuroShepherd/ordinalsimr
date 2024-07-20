#' row_delete UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_row_delete_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("delete_row"), "Delete Row")
  )
}

#' row_delete Server Functions
#'
#' @noRd
mod_row_delete_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    delete_row_button <- reactive({
      input$delete_row
    })

    return(delete_row_button)
  })
}

## To be copied in the UI
# mod_row_delete_ui("row_delete_1")

## To be copied in the server
# mod_row_delete_server("row_delete_1")
