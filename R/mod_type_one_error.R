#' type_one_error UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_type_one_error_ui <- function(id) {
  ns <- NS(id)
  tagList(
    markdown("Choose whether you would like to calculate the Type I Error for both groups, Group 1, Group 2, or None of the groups. Default and recommended is to calculate for Both groups."),
    radioButtons(
      ns("t1_error_toggle"), "Calculate Type I Error",
      c("Both" = "both", "Group 1" = "group1", "Group 2" = "group2", "None" = "none")
    )
  )
}

#' type_one_error Server Functions
#'
#' @noRd
mod_type_one_error_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    t1_error_toggle <- reactive({
      input$t1_error_toggle
    })

    return(t1_error_toggle)
  })
}

## To be copied in the UI
# mod_type_one_error_ui("type_one_error_1")

## To be copied in the server
# mod_type_one_error_server("type_one_error_1")
