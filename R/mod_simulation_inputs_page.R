#' simulation_inputs_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_simulation_inputs_page_ui <- function(id){
  ns <- NS(id)
  nav_panel(
    "Simulation Inputs",
    layout_columns(
      col_widths = c(5, 7, -2, 8, -2),
      row_heights = c(3, 2),
      navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Inputs",
        nav_panel(
          "Core Inputs",
          sliderInput(ns("type1_error"), "Fillter", min = 0, max = 1, value = 0.05)
        ),
        nav_panel(
          "Type I Erorr",
          radioButtons(
            ns("t1_error_toggle"), "Calculate Type I Error",
            c("Both" = "both", "Group 1" = "group1", "Group 2" = "group2", "None" = "none")
          )
        ),
        nav_panel(
          "RNG Options",
          markdown("These Random Number Generators are advanced options, and they use the default values employed by R as of version 4.4. Run `?RNGkind` in an R session to see the associated help file."),
          selectInput(ns("rng"), "RNG", choices = c("L'Ecuyer-CMRG", "Mersenne-Twister"), selected = "L'Ecuyer-CMRG"),
          selectInput(ns("rng"), "RNG", choices = c("L'Ecuyer-CMRG", "Mersenne-Twister"), selected = "L'Ecuyer-CMRG"),
          selectInput(ns("rng"), "RNG", choices = c("L'Ecuyer-CMRG", "Mersenne-Twister"), selected = "L'Ecuyer-CMRG")
        ),
        nav_panel(
          shiny::icon("circle-info"),
          markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)")
        )
      ),
      card(
        card_header("Data Entry", class = "bg-dark"),
        dataTableOutput(ns("data_table")),
        card_body(
          fillable = FALSE,
          actionButton(ns("add_row"), "Add Row"),
          actionButton(ns("delete_row"), "Delete Row")
          )
      ),
      card(
        card_header("Data Tables", class = "bg-dark")
      )

    )
  )
}

#' simulation_inputs_page Server Functions
#'
#' @noRd
mod_simulation_inputs_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

})
}

## To be copied in the UI
# mod_simulation_inputs_page_ui("simulation_inputs_page_1")

## To be copied in the server
# mod_simulation_inputs_page_server("simulation_inputs_page_1")
