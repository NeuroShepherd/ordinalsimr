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
#' @import shinycssloaders
mod_plot_distributions_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 3,
      sliderInput(ns("remove_outlier_percentage"), "Remove top x% from view",
                  min = 0, max = 100, value = 10, round = TRUE),
      numericInput(ns("user_p_val"), "p-value Threshold",
                   min = 0, max = 1, value = 0.05, step = 0.01),
      numericInput(ns("power_confidence_int"), "%CI: Power",
                   min = 0, max = 100, value = 95, step = 1),
      numericInput(ns("t1_error_group1_confidence_int"), "%CI: Type 1 Error, Group 1",
                   min = 0, max = 100, value = 95, step = 1),
      numericInput(ns("t1_error_group2_confidence_int"), "%CI: Type 1 Error, Group 2",
                   min = 0, max = 100, value = 95, step = 1)
    ),
    box(
      width = 9,
      shinycssloaders::withSpinner(plotOutput(ns("distribution_plot_results")), type = 2,color.background = "#0275D8"),
      br(),
      tabsetPanel(type = "tabs",
                  tabPanel("Power and Type II Error",
                           shinycssloaders::withSpinner(DT::dataTableOutput(ns("distribution_statistics")), type=8)),
                  tabPanel("Type I Error: Group 1",
                           shinycssloaders::withSpinner(DT::dataTableOutput(ns("t1_error_group1")), type = 8)),
                  tabPanel("Type I Error: Group 2",
                           shinycssloaders::withSpinner(DT::dataTableOutput(ns("t1_error_group2")), type = 8))
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
    p_value_reactive_table <- reactive({ as.data.frame(p_value_table$comparison_results()$p_values) })


    # COMPARISON STATISTICS/GRAPHING
    # !!!plot!!!
    distribution_plot <- reactive({
      p_value_reactive_table() %>%
        plot_distribution_results(outlier_removal = outlier_percent_removal(),
                                  alpha = p_val_threshold())})
    output$distribution_plot_results <- renderPlot({
      distribution_plot()
    })
      # ensure evaluation in case user goes directly to downloading the results
    outputOptions(output, "distribution_plot_results", suspendWhenHidden = FALSE)

    # !!!statistics!!!
    distribution_statistics <- reactive({p_value_reactive_table() %>%
      calculate_power_t2error(alpha = p_val_threshold(),
                              n = n(),
                              power_confidence_int = input$power_confidence_int)
      })
    output$distribution_statistics <- DT::renderDataTable({
      distribution_statistics() %>%
        select(-.data$lower_power_bound, -.data$upper_power_bound) %>%
        rename(`Statistical Test` = .data$test,
               "Power (1-\U03B2)" = .data$power,
               "Type II Error (\U03B2)" = .data$t2_error) %>%
        DT::datatable() %>%
        DT::formatRound(c(2,4), 5)
    })
    outputOptions(output, "distribution_statistics", suspendWhenHidden = FALSE)


    # GROUP 1 TYPE 1 ERROR
    group1_t1_reactive_table <- reactive({ p_value_table$group1_results()$p_values %>%
        as.data.frame() %>%
        calculate_t1_error(t1_error_confidence_int = input$t1_error_group1_confidence_int)
      })
    output$t1_error_group1 <- DT::renderDataTable({
      group1_t1_reactive_table() %>%
        select(-.data$lower_t1_bound, -.data$upper_t1_bound) %>%
        rename(`Statistical Test` = .data$test,
               "Type I Error (\U003B1)" = .data$t1_error) %>%
        DT::datatable() %>%
        DT::formatRound(c(2), 5)

    })
    outputOptions(output, "t1_error_group1", suspendWhenHidden = FALSE)


    # GROUP 2 TYPE 1 ERROR
    group2_t1_reactive_table <- reactive({ p_value_table$group2_results()$p_values %>%
        as.data.frame() %>%
        calculate_t1_error(t1_error_confidence_int = input$t1_error_group2_confidence_int)
      })
    output$t1_error_group2 <- DT::renderDataTable({
      group2_t1_reactive_table() %>%
        select(-.data$lower_t1_bound, -.data$upper_t1_bound) %>%
        rename(`Statistical Test` = .data$test,
               "Type I Error (\U003B1)" = .data$t1_error) %>%
        DT::datatable() %>%
        DT::formatRound(c(2), 5)
    })
    outputOptions(output, "t1_error_group2", suspendWhenHidden = FALSE)


    return(list(
      distribution_statistics = distribution_statistics,
      distribution_plot = distribution_plot,
      group1_t1error = group1_t1_reactive_table,
      group2_t1error = group2_t1_reactive_table
    ))

  })
}

## To be copied in the UI
# mod_plot_distributions_ui("plot_distributions_1")

## To be copied in the server
# mod_plot_distributions_server("plot_distributions_1")
