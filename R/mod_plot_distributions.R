#' plot_distributions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom shiny NS tagList
mod_plot_distributions_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 3,
      sliderInput(ns("remove_outlier_percentage"), "Remove top x% from view",
                  min = 0, max = 100, value = 10, round = TRUE),
      numericInput(ns("user_p_val"), "p-value Threshold",
                   min = 0, max = 1, value = 0.05, step = 0.01),
      numericInput(ns("power_confidence_int"), "Power Confidence Interval",
                   min = 0, max = 100, value = 95, step = 1)
    ),
    box(
      width = 9,
      plotOutput(ns("distribution_plot_results")),
      br(),
      tabsetPanel(type = "tabs",
                  tabPanel("Power and T2 Error", DT::dataTableOutput(ns("distribution_statistics"))),
                  tabPanel("T1 Error: Group 1"),
                  tabPanel("T1 Error: Group 2")
                  )

    )
  )
}

#' plot_distributions Server Functions
#'
#' @noRd
mod_plot_distributions_server <- function(id, p_value_table, n){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    outlier_percent_removal <- reactive({ (100 - input$remove_outlier_percentage)/100 })
    p_val_threshold <- reactive({ input$user_p_val })
    p_value_reactive_table <- reactive({ as.data.frame(p_value_table$comparison()$p_values) })


    output$distribution_plot_results <- renderPlot({
      p_value_reactive_table() %>%
        plot_distribution_results(outlier_removal = outlier_percent_removal(),
                                  alpha = p_val_threshold())
    })


    output$distribution_statistics <- DT::renderDataTable({
      p_value_reactive_table() %>%
        calculate_power_t2error(alpha = p_val_threshold(),
                                n = n(),
                                power_confidence_int = input$power_confidence_int) %>%
        select(-lower_power_se_value, -upper_power_se_value) %>%
        rename(`Statistical Test` = name,
               `Power (1-β)` := power_value,
               `Type II Error (β)` = t2_error_value) %>%
        DT::datatable() %>%
        formatRound(c(2,4), 5)
    })


  })
}

## To be copied in the UI
# mod_plot_distributions_ui("plot_distributions_1")

## To be copied in the server
# mod_plot_distributions_server("plot_distributions_1")
