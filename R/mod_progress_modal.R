#' progress_modal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_progress_modal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("show_progress_modal"), "Show Progress")
  )
}

#' progress_modal Server Functions
#'
#' @noRd
mod_progress_modal_server <- function(id, open_modal_from_sim_start){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent({input$show_progress_modal; open_modal_from_sim_start()}, {
      showModal(modalDialog(
        title = "Progress Status",
        fluidRow(
          mod_stats_calculations_ui("stats_calculations_1")[["comparison_progress"]],
          br(),
          mod_stats_calculations_ui("stats_calculations_1")[["group1_progress"]],
          br(),
          mod_stats_calculations_ui("stats_calculations_1")[["group2_progress"]]
        ),
        easyClose = TRUE,
        footer = NULL,
        size = "xl"
      ))
    })

  })
}

## To be copied in the UI
# mod_progress_modal_ui("progress_modal_1")

## To be copied in the server
# mod_progress_modal_server("progress_modal_1")
