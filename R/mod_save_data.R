#' save_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_save_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    downloadButton(ns("save_button"), "Save Results")
  )
}

#' save_data Server Functions
#'
#' @noRd
mod_save_data_server <- function(id, data, input, output, session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    results_data <- reactive({ data })


    # volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    # shinyFileSave(input, "save_results", roots = "home")

    output$save_button <- downloadHandler(
      filename = function() {
        # Use .Rdata extension as it allows saving of multiple objects unlike
        # .rds; this function may save more than just the results in the future.
        # possibly consider using the session token, session$token, in name
        glue::glue("data-{Sys.Date()}-{session$token}.rds")
      },
      content = function(file) {
        saveRDS(results_data(), file)
      }
    )

  })
}

## To be copied in the UI
# mod_save_data_ui("save_data_1")

## To be copied in the server
# mod_save_data_server("save_data_1")
