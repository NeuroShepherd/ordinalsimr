#' stats_calculations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_stats_calculations_ui <- function(id){
  ns <- NS(id)

  tagList(
    box(
      width = 3,
      column(12, align = "center",
             radioButtons(ns("t1_error_toggle"), "Calculate Type I Error",
                          c("Both" = "both", "Group 1" = "group1", "Group 2" = "group2", "None" = "none")),
             actionButton(ns("run_button"), "Run Tests")
             )
    ),
    box(
      width = 9,
      DT::dataTableOutput(ns("results_table"))
    )
  )

}

#' stats_calculations Server Functions
#'
#' @noRd
mod_stats_calculations_server <- function(id, probability_data, sample_prob, iterations, sample_size){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # collect parameters in a reactive list
    parameters <- reactive({
      list(
        prob0 = dplyr::pull(probability_data(), "Group 1 Probabilities"),
        prob1 = dplyr::pull(probability_data(), "Group 2 Probabilities"),
        sample_prob = sample_prob(),
        iterations = iterations(),
        sample_size = sample_size()
        )
      })


    # NOTE: the whole list is reactive, and need to subset elements after
    # calling reactivity
    # usage example: parameters()$null_probs

    comparison_results <- eventReactive(input$run_button, {
      run_simulations(parameters()$sample_size,
                      prob0 = parameters()$prob0,
                      sample_prob = parameters()$sample_prob,
                      prob1 = parameters()$prob1,
                      niter = parameters()$iterations)
    })

    group1_results <- eventReactive(input$run_button, {
      if (input$t1_error_toggle %in% c("both", "group1")) {
        run_simulations(parameters()$sample_size,
                      prob0 = parameters()$prob0,
                      sample_prob = parameters()$sample_prob,
                      prob1 = parameters()$prob0,
                      niter = parameters()$iterations)
      } else (data.frame())
    })

    group2_results <- eventReactive(input$run_button, {
      if (input$t1_error_toggle %in% c("both", "group2")) {
        run_simulations(parameters()$sample_size,
                        prob0 = parameters()$prob1,
                        sample_prob = parameters()$sample_prob,
                        prob1 = parameters()$prob1,
                        niter = parameters()$iterations)
      } else (data.frame())
    })


    output$results_table <- DT::renderDataTable(
      DT::datatable(data = as.data.frame(comparison_results()$p_values),
                    options = list(scrollX = TRUE)
      )
    )

    return(list(comparison_results = comparison_results,
                group1_results = group1_results,
                group2_results = group2_results
                ))
  })
}

## To be copied in the UI
# mod_stats_calculations_ui("stats_calculations_1")

## To be copied in the server
# mod_stats_calculations_server("stats_calculations_1")
