#' plot_distributions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_distributions_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title = "Plot Results",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          # action button
          sliderInput(ns("remove_outlier_percentage"), "Filter out top % of observations",
                      min = 0, max = 100, value = 10, round = TRUE)
        ),
        mainPanel(
          plotOutput(ns("distribution_plot_results"))
        )
      )
    )

  )
}

#' plot_distributions Server Functions
#'
#' @noRd
mod_plot_distributions_server <- function(id, p_value_table){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    outlier_percent_removal <- reactive({ (100 - input$remove_outlier_percentage)/100 })

    p_value_reactive_table <- reactive({ p_value_table })

    output$distribution_plot_results <- renderPlot({
      p_value_reactive_table() %>%
        as.data.frame() %>%
        pivot_longer(cols = everything(),names_to = "test_name") %>%
        slice_min(order_by = value, prop = outlier_percent_removal() ) %>%
        ggplot(aes(x = value, fill = test_name )) +
        geom_density(alpha = 0.5, color = "black")
    })


  })
}

## To be copied in the UI
# mod_plot_distributions_ui("plot_distributions_1")

## To be copied in the server
# mod_plot_distributions_server("plot_distributions_1")
