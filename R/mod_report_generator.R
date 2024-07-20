#' report_generator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_generator_ui <- function(id) {
  ns <- NS(id)
  list(
    update_report = actionButton(ns("update_report"), "Update Report"),
    download_report = actionButton(ns("download_report"), "Download Report"),
    rendered_report = fluidPage(uiOutput(ns("rendered_report")))
  )
}

#' report_generator Server Functions
#'
#' @noRd
mod_report_generator_server <- function(id, formatted_data, rng_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    custom_report <- eventReactive(input$update_report, {
      HTML(
        readLines(
          rmarkdown::render(
            system.file("report_template.Rmd", package = "ordinalsimr"),
            quiet = TRUE,
            output_format = rmarkdown::html_fragment(),
            params = list(
              data_object = formatted_data()$comparison_data$distribution_plot,
              rng_info = list(
                rng_info$rng_kind(),
                rng_info$rng_normal_kind(),
                rng_info$rng_sample_kind()
              )
            )
          )
        )
      )
    })


    output$rendered_report <- renderUI({
      custom_report()
    })
  })
}

## To be copied in the UI
# mod_report_generator_ui("report_generator_1")

## To be copied in the server
# mod_report_generator_server("report_generator_1")
