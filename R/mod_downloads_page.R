#' downloads_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_downloads_page_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Report & Downloads",
    layout_columns(
      col_widths = c(2, 10, 2),
      fillable = TRUE,
      card(
        card_header(
          "Update and Download Report",
          class = "bg-dark"
        ),
        fill = TRUE,
        mod_report_generator_ui("report_generator_1")[["update_report"]],
        mod_report_generator_ui("report_generator_1")[["download_report"]]
      ),
      card(
        card_header(
          "Report",
          class = "bg-dark"
        ),
        fill = TRUE,
        mod_report_generator_ui("report_generator_1")[["rendered_report"]]
      ),
      card(
        card_header(
          "Download Data",
          class = "bg-dark"
        ),
        fill = TRUE,
        mod_save_data_ui("save_data_1")[["save_rds"]],
        mod_save_data_ui("save_data_1")[["save_excel"]]
      ),
    )
  )
}

#' downloads_page Server Functions
#'
#' @noRd
mod_downloads_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_downloads_page_ui("downloads_page_1")

## To be copied in the server
# mod_downloads_page_server("downloads_page_1")
