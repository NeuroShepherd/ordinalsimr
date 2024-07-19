#' distributions_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_distributions_page_ui <- function(id){
  ns <- NS(id)
  nav_panel(
    "Simulation Inputs",
    layout_columns(
      col_widths = c(5 , 7, -2, 8, -2),
      row_heights = c(3, 3),
      navset_card_tab(
        full_screen = TRUE,
        title = "Inputs",
        nav_panel(
          "Core Inputs",
          layout_column_wrap(
            width = 1/2,
            heights_equal = "row",
            card(
              !!!mod_plot_distributions_ui("plot_distributions_1")[["ci_inputs"]]
            ),
            card(
              mod_plot_distributions_ui("plot_distributions_1")[["p_val_input"]]
            )
          )
        ),
        nav_panel(
          "Type I Erorr",
          markdown("something here")
        )
      ),
      card(
        card_header("Data Entry", class = "bg-dark"),
        markdown("som"),
        card_body(
          fillable = FALSE,
          class = "gap-2 container",
          markdown("something here")
        ),
        markdown("something here")
      ),
      navset_card_pill(
        title = "Results",
        !!!mod_plot_distributions_ui("plot_distributions_1")[["output_data"]]
      )

    )

  )
}

#' distributions_page Server Functions
#'
#' @noRd
mod_distributions_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_distributions_page_ui("distributions_page_1")

## To be copied in the server
# mod_distributions_page_server("distributions_page_1")
