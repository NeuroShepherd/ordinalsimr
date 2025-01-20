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
    download_report = downloadButton(ns("download_report"), "Download Report"),
    rendered_report = fluidPage(uiOutput(ns("rendered_report")))
  )
}

#' report_generator Server Functions
#'
#' @noRd
mod_report_generator_server <- function(id, formatted_data, rng_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    custom_report <- reactive({
      HTML(
        readLines(
          rmarkdown::render(
            system.file("report_template.Rmd", package = "ordinalsimr"),
            quiet = TRUE,
            output_format = rmarkdown::html_fragment(),
            params = list(
              from_shiny_app = TRUE,
              comparison_data = list(
                run_info = formatted_data()$comparison_data$run_info,
                distribution_statistics = formatted_data()$comparison_data$distribution_statistics,
                distribution_plot = formatted_data()$comparison_data$distribution_plot
              ),
              group1_data = list(
                run_info = formatted_data()$group1_data$run_info,
                group1_t1error = formatted_data()$group1_data$group1_t1error
              ),
              group2_data = list(
                run_info = formatted_data()$group2_data$run_info,
                group2_t1error = formatted_data()$group2_data$group2_t1error
              ),
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


    download_counter_zip <- reactiveVal(1)
    output$download_report <- downloadHandler(
      filename = function() {
        paste0(
          "ordinalsimr_session_", strtrim(session$token, 8),
          "_download_", download_counter_zip(),
          ".zip"
        )
      },
      content = function(file) {
        output_folder <- file.path(paste0(
          "ordinalsimr_session_", strtrim(session$token, 8),
          "_download_", download_counter_zip()
        ))
        dir.create(output_folder, showWarnings = FALSE)

        try({
          write(
            custom_report(),
            file.path(output_folder, "completed_report.html")
          )
        })
        try({
          saveRDS(
            formatted_data(),
            file.path(output_folder, "ordinalsimr_results.rds")
          )
        })
        try({
          write(
            readLines(system.file("report_template.Rmd", package = "ordinalsimr")),
            file.path(output_folder, "report_template.Rmd")
          )
        })

        zip_files <- c(
          file.path(output_folder, "completed_report.html"),
          file.path(output_folder, "ordinalsimr_results.rds"),
          file.path(output_folder, "report_template.Rmd")
        )

        try({
          utils::zip(
            zipfile = file,
            files = zip_files
            # flags = '-r9Xb'
          )
        })

        unlink(output_folder, recursive = TRUE, force = TRUE)
        download_counter_zip(download_counter_zip() + 1)
      },
      contentType = "application/zip"
    )
  })
}

## To be copied in the UI
# mod_report_generator_ui("report_generator_1")

## To be copied in the server
# mod_report_generator_server("report_generator_1")
