#' test_pass_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_test_pass_data_ui <- function(id){
  ns <- NS(id)
  tabPanel(title = "Hello There",
    shiny::dataTableOutput(ns("hello_there"))
  )
}

#' test_pass_data Server Functions
#'
#' @noRd
mod_test_pass_data_server <- function(id, prob_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$hello_there <- shiny::renderDataTable(prob_data())
  })
}

## To be copied in the UI
# mod_test_pass_data_ui("test_pass_data_1")

## To be copied in the server
# mod_test_pass_data_server("test_pass_data_1")



# Code that finally helped me figure it out. Three key parts:
# 1. return() the reactive data from the server generating the data.
# In this case, return(probability_data) from mod_data_entry_server.
# 2. Within app_server(), save mod_data_entry_server("id") to an object.
# !!It will still be able to provide output to the UI even like this!!
# 3. In the server module needing access to the data, expose an argument
# to pass through the object saved in the previous step. (Note that the
# argument passed through need to be treated as a function i.e. arg().)
# https://stackoverflow.com/questions/58899369/reactive-shiny-modules-sharing-data


