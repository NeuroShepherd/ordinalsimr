#' stats_calculations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_stats_calculations_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title = "New Data Entries",
    textOutput(ns("testing"))

  )
}

#' stats_calculations Server Functions
#'
#' @noRd
mod_stats_calculations_server <- function(id, probability_data, iterations, sample_size){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # collect parameters in a reactive list
    parameters <- reactive({
        list(
        null_probs = dplyr::pull(probability_data(), "Null Group Probabilities"),
        int_probs = dplyr::pull(probability_data(), "Intervention Group Probs."),
        iterations = iterations()
        #, sample_size = sample_size()
        )
      })


    # usage example: parameters()$null_probs
    # NOTE: the whole list is reactive, and need to subset elements after
    # calling reactivity

    output$testing <- shiny::renderText({
      c(
        # parameters()$null_probs,
        # parameters()$int_probs,
        parameters()$iterations
        )
      })


  })
}

## To be copied in the UI
# mod_stats_calculations_ui("stats_calculations_1")

## To be copied in the server
# mod_stats_calculations_server("stats_calculations_1")
