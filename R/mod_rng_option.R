#' rng_option UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_rng_option_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' rng_option Server Functions
#'
#' @noRd 
mod_rng_option_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_rng_option_ui("rng_option_1")
    
## To be copied in the server
# mod_rng_option_server("rng_option_1")
