#' select_tests UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_tests_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxGroupInput(
      ns("included"),
      label = "Tests",
      choices = c(
        "Wilcoxon", "Fisher", "Chi Squared (No Correction)",
        "Chi Squared (Correction)",
        "Proportional Odds" = "Prop. Odds",
        "Coin Independence Test" = "Coin Indep. Test"
      ),
      selected = c(
        "Wilcoxon", "Fisher", "Chi Squared (No Correction)",
        "Chi Squared (Correction)",
        "Proportional Odds" = "Prop. Odds",
        "Coin Independence Test" = "Coin Indep. Test"
      )
    )
  )
}

#' select_tests Server Functions
#'
#' @noRd
mod_select_tests_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_tests <- reactive({
      input$included
    })

    return(selected_tests)
  })
}

## To be copied in the UI
# mod_select_tests_ui("select_tests_1")

## To be copied in the server
# mod_select_tests_server("select_tests_1")
