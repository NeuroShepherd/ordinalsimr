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
    shiny::textInput(ns("sample_n"), "Sample Size", value = 80)
  )
}

#' sample_size Server Functions
#'
#' @noRd
mod_sample_size_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    sample_n <- reactive({
      # should technically set up checks that only a numeric vector is
      # being passed before evaluating the expression (or, that the string
      # being passed is comma-separated values with `:` and seq*()
      # also being allowed.)
      unique(eval(parse(text = paste0("c(", input$sample_n, ")") )))
      })

    return(sample_n)

  })
}

## To be copied in the UI
# mod_sample_size_ui("sample_size_1")

## To be copied in the server
# mod_sample_size_server("sample_size_1")
