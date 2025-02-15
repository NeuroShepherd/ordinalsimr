#' kill_simulations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_kill_simulations_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("kill_simulations"), "Kill Simulations") %>%
      tagAppendAttributes(class = "btn btn-danger")
  )
}

#' kill_simulations Server Functions
#'
#' @noRd
mod_kill_simulations_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_kill_simulations_ui("kill_simulations_1")

## To be copied in the server
# mod_kill_simulations_server("kill_simulations_1")
