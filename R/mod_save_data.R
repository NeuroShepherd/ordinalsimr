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
mod_save_data_server <- function(id, input_data, processed_data, input, output, session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    # shinyFileSave(input, "save_results", roots = "home")

    data_to_save <- reactive({
      list(
        comparison_data = format_simulation_data(input_data$comparison_results()) %>%
          append(list(distribution_statistics = processed_data$distribution_statistics(),
                      distribution_plot = processed_data$distribution_plot()
                      )),
        group1_data = format_simulation_data(input_data$group1_results()) %>%
          append(list(group1_t1error = processed_data$group1_t1error())),
        group2_data = format_simulation_data(input_data$group1_results()) %>%
          append(list(group2_t1error = processed_data$group2_t1error()))
        )
      })


    download_counter <- reactiveVal(1)
    output$save_button <- downloadHandler(
      filename = function() {
        # Consider: use .RData in future for flexibility?
        glue::glue("data-{Sys.Date()}-{session$token}-{download_counter()}.rds")
      },
      content = function(file) {
        saveRDS(data_to_save(), file)
        # increment download number
        download_counter(download_counter() + 1)
      }
    )


  })
}

## To be copied in the UI
# mod_save_data_ui("save_data_1")

## To be copied in the server
# mod_save_data_server("save_data_1")
