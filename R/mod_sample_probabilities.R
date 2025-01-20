#' sample_probabilities UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sample_probabilities_ui <- function(id) {
  ns <- NS(id)

  default_ratio <- tryCatch(
    getOption("ordinalsimr.default_ratio", default = "50:50"),
    error = function(e) {
      "50:50"
    }
  )

  tagList(
    textInput(ns("sample_probabilities"),
      "Distribution Ratio (Control:Intervention)",
      value = default_ratio,
      placeholder = "Enter desired control:intervention group ratio"
    )
  )
}

#' sample_probabilities Server Functions
#'
#' @noRd
mod_sample_probabilities_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sample_probabilities <- reactive({
      parse_ratio_text(input$sample_probabilities)
    })

    return(sample_probabilities)
  })
}

## To be copied in the UI
# mod_sample_probabilities_ui("sample_probabilities_1")

## To be copied in the server
# mod_sample_probabilities_server("sample_probabilities_1")
