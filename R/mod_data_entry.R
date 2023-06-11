#' data_entry UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_entry_ui <- function(id){
  ns <- NS(id)

    tabPanel(title = "Data Input",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   # something here? data upload option?
                 ),
                 mainPanel(
                   rhandsontable::rHandsontableOutput(ns("hottable"))
                 )
               )
             ))

}

#' data_entry Server Functions
#'
#' @noRd
mod_data_entry_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    values = reactiveValues()

    data = reactive({

      if (!is.null(input$hottable)) {
        DF = rhandsontable::hot_to_r(input$hottable)
      } else {
        if (is.null(values[["DF"]])) {

          # create a placehodler for generating the number of rows in the data table
          # which will be based on the number of possible outcomes
          # table_row_number <- input$number_of_outcomes

          DF = data.frame(`Null Group Probabilities` = rep(0,10),
                          `Intervention Group Probs.` = rep(0,10),
                          check.names = FALSE)
        } else
          DF = values[["DF"]]
      }

      values[["DF"]] = DF
      DF

    })

    output$hottable <- rhandsontable::renderRHandsontable({
      DF = data()
      if (!is.null(DF))
        rhandsontable::rhandsontable(DF, stretchH = "all")
    })



  })
}

## To be copied in the UI
# mod_data_entry_ui("data_entry_1")

## To be copied in the server
# mod_data_entry_server("data_entry_1")
