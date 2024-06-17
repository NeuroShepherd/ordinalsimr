#' report_generator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_report_generator_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' report_generator Server Functions
#'
#' @noRd 
mod_report_generator_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_report_generator_ui("report_generator_1")
    
## To be copied in the server
# mod_report_generator_server("report_generator_1")
