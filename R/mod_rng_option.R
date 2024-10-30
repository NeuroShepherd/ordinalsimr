#' rng_option UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rng_option_ui <- function(id) {
  ns <- NS(id)

  list(
    selectInput(ns("rng_kind"),
      label = "RNG Kind",
      choices = c(
        "Mersenne-Twister", "Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
        "Knuth-TAOCP-2002", "Knuth-TAOCP", "L'Ecuyer-CMRG"
      ),
      selected = "Mersenne-Twister"
    ),
    selectInput(ns("rng_normal_kind"),
      label = "RNG Normal Kind",
      choices = c("Inversion", "Kinderman-Ramage", "Buggy Kinderman-Ramage", "Ahrens-Dieter", "Box-Muller"),
      selected = "Inversion"
    ),
    selectInput(ns("rng_sample_kind"),
      label = "RNG Sample Kind",
      choices = c("Rejection", "Rounding"),
      selected = "Rejection"
    )
  )
}

#' rng_option Server Functions
#'
#' @noRd
mod_rng_option_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rng_kind <- reactive({
      as.character(input$rng_kind)
    })
    rng_normal_kind <- reactive({
      as.character(input$rng_normal_kind)
    })
    rng_sample_kind <- reactive({
      as.character(input$rng_sample_kind)
    })

    return(list(
      rng_kind = rng_kind,
      rng_normal_kind = rng_normal_kind,
      rng_sample_kind = rng_sample_kind
    ))
  })
}

## To be copied in the UI
# mod_rng_option_ui("rng_option_1")

## To be copied in the server
# mod_rng_option_server("rng_option_1")
