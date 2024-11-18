#' save_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_save_data_ui <- function(id) {
  ns <- NS(id)
  list(
    save_rds = downloadButton(ns("save_button"), "Save Results as .rds"),
    save_excel = uiOutput(ns("save_xlsx_ui"))
  )
}

#' save_data Server Functions
#'
#' @noRd
mod_save_data_server <- function(id, input_data, processed_data, rng_info, input, output, session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    # shinyFileSave(input, "save_results", roots = "home")

    data_to_save <- reactive({
      list(
        comparison_data = list(
          run_info = bind_rows(input_data$comparison_results()),
          distribution_statistics = processed_data$distribution_statistics(),
          distribution_plot = processed_data$distribution_plot()
        ),
        group1_data = list(
          run_info = bind_rows(input_data$group1_results()),
          group1_t1error = processed_data$group1_t1error()
        ),
        group2_data = list(
          run_info = bind_rows(input_data$group1_results()),
          group2_t1error = processed_data$group2_t1error()
        )
      )
    })


    download_counter_rds <- reactiveVal(1)
    output$save_button <- downloadHandler(
      filename = function() {
        # Consider: use .RData in future for flexibility?
        paste0("data-", Sys.Date(), "-", strtrim(session$token, 6), "-", download_counter_rds(), ".rds")
      },
      content = function(file) {
        saveRDS(data_to_save(), file)
        # increment download number
        download_counter_rds(download_counter_rds() + 1)
      }
    )

    output$save_xlsx_ui <- renderUI({
      if (requireNamespace("writexl", quietly = TRUE)) {
        downloadButton(ns("save_xlsx"), "Save Results as .xlsx", style = "width:100%;")
      } else {
        tagList(
          h5("Excel Download:"),
          p("Please install the {writexl} package.")
        )
      }
    })

    download_counter_excel <- reactiveVal(1)
    output$save_xlsx <- downloadHandler(
      filename = function() {
        paste0("data-", Sys.Date(), "-", strtrim(session$token, 6), "-", download_counter_excel(), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(
          list(
            distribution_statistics = data_to_save()$comparison_data$distribution_statistics,
            comparison_run_info = data_to_save()$comparison_data$run_info,
            group1_type1_error = data_to_save()$group1_data$group1_t1error,
            group1_run_info = data_to_save()$group1_data$run_info,
            group2_type1_error = data_to_save()$group2_data$group2_t1error,
            group2_run_info = data_to_save()$group2_data$run_info
          ) %>%
            lapply(function(x) {
              if (is.null(x)) {
                data.frame()
              } else {
                x
              }
            }),
          path = file
        )
        # increment download number
        download_counter_excel(download_counter_excel() + 1)
      }
    )

    return(data_to_save)
  })
}

## To be copied in the UI
# mod_save_data_ui("save_data_1")

## To be copied in the server
# mod_save_data_server("save_data_1")
