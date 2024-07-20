#' navbar_extras UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_navbar_extras_ui <- function(id) {
  ns <- NS(id)

  list(
    nav_item(
      tags$a(
        shiny::icon("book-open-reader"),
        "Docs",
        href = "https://neuroshepherd.github.io/ordinalsimr/",
        target = "_blank"
      )
    ),
    nav_item(
      tags$a(
        shiny::icon("circle-exclamation"),
        "Issues",
        href = "https://github.com/neuroshepherd/ordinalsimr/issues",
        target = "_blank"
      )
    ),
    nav_item(
      tags$a(
        shiny::icon("github"),
        "GitHub",
        href = "https://github.com/neuroshepherd/ordinalsimr",
        target = "_blank"
      )
    )
  )
}

#' navbar_extras Server Functions
#'
#' @noRd
mod_navbar_extras_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_navbar_extras_ui("navbar_extras_1")

## To be copied in the server
# mod_navbar_extras_server("navbar_extras_1")
