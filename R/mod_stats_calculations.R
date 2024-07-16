#' stats_calculations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_stats_calculations_ui <- function(id) {
  ns <- NS(id)

  tagList(
    box(
      width = 3,
      column(12,
        align = "center",
        radioButtons(
          ns("t1_error_toggle"), "Calculate Type I Error",
          c("Both" = "both", "Group 1" = "group1", "Group 2" = "group2", "None" = "none")
        ),
        actionButton(ns("run_button"), "Run Tests")
      )
    ),
    box(
      width = 9,
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Comparison p-values",
          shinycssloaders::withSpinner(DT::dataTableOutput(ns("results_table")), type = 8)
        ),
        tabPanel(
          "Group 1 p-values",
          shinycssloaders::withSpinner(DT::dataTableOutput(ns("group1_pvalues")), type = 8)
        ),
        tabPanel(
          "Group 2 p-values",
          shinycssloaders::withSpinner(DT::dataTableOutput(ns("group2_pvalues")), type = 8)
        )
      )
    )
  )
}

#' stats_calculations Server Functions
#'
#' @noRd
mod_stats_calculations_server <- function(id, probability_data, sample_prob, iterations, sample_size, rng_info, included_tests) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # collect parameters in a reactive list
    parameters <- reactive({
      list(
        prob0 = dplyr::pull(probability_data(), "Group 1 Probabilities"),
        prob1 = dplyr::pull(probability_data(), "Group 2 Probabilities"),
        sample_prob = sample_prob(),
        iterations = iterations(),
        sample_size = sample_size(),
        included_tests = included_tests()
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
        niter = parameters()$iterations,
        included = parameters()$included_tests,
        .rng_kind = rng_info$rng_kind(),
        .rng_normal_kind = rng_info$rng_normal_kind(),
        .rng_sample_kind = rng_info$rng_sample_kind()
      )
    })

    group1_results <- eventReactive(input$run_button, {
      if (input$t1_error_toggle %in% c("both", "group1")) {
        run_simulations(parameters()$sample_size,
          prob0 = parameters()$prob0,
          sample_prob = parameters()$sample_prob,
          prob1 = parameters()$prob0,
          niter = parameters()$iterations,
          included = parameters()$included_tests,
          .rng_kind = rng_info$rng_kind(),
          .rng_normal_kind = rng_info$rng_normal_kind(),
          .rng_sample_kind = rng_info$rng_sample_kind()
        )
      } else {
        (data.frame())
      }
    })

    group2_results <- eventReactive(input$run_button, {
      if (input$t1_error_toggle %in% c("both", "group2")) {
        run_simulations(parameters()$sample_size,
          prob0 = parameters()$prob1,
          sample_prob = parameters()$sample_prob,
          prob1 = parameters()$prob1,
          niter = parameters()$iterations,
          included = parameters()$included_tests,
          .rng_kind = rng_info$rng_kind(),
          .rng_normal_kind = rng_info$rng_normal_kind(),
          .rng_sample_kind = rng_info$rng_sample_kind()
        )
      } else {
        (data.frame())
      }
    })


    output$results_table <- DT::renderDataTable({
      comp_res <- comparison_results() %>%
        bind_rows() %>%
        dplyr::select(.data$sample_size,
                      dplyr::any_of(c(
                        "Wilcoxon", "Fisher", "Chi Squared\n(No Correction)",
                        "Chi Squared\n(Correction)", "Prop. Odds", "Coin Indep. Test"
                      )))

      comp_res %>%
        DT::datatable(options = list(scrollX = TRUE)) %>%
        DT::formatRound(2:ncol(comp_res), digits = 5)
    })
    outputOptions(output, "results_table", suspendWhenHidden = FALSE)

    # if not keeping these output tables, use observe({group1_results()}) to
    # ensure evaluation
    output$group1_pvalues <- DT::renderDataTable({
      g1_res <- group1_results() %>%
        bind_rows() %>%
        dplyr::select(.data$sample_size,
                      dplyr::any_of(c(
                        "Wilcoxon", "Fisher", "Chi Squared\n(No Correction)",
                        "Chi Squared\n(Correction)", "Prop. Odds", "Coin Indep. Test"
                      )))

      g1_res %>%
        DT::datatable(options = list(scrollX = TRUE)) %>%
        DT::formatRound(2:ncol(g1_res), digits = 5)
    })
    outputOptions(output, "group1_pvalues", suspendWhenHidden = FALSE)

    output$group2_pvalues <- DT::renderDataTable({
      g2_res <- group2_results() %>%
        bind_rows() %>%
        dplyr::select(.data$sample_size,
                      dplyr::any_of(c(
                        "Wilcoxon", "Fisher", "Chi Squared\n(No Correction)",
                        "Chi Squared\n(Correction)", "Prop. Odds", "Coin Indep. Test"
                      )))

      g2_res %>%
        DT::datatable(options = list(scrollX = TRUE)) %>%
        DT::formatRound(2:ncol(g2_res), digits = 5)
    })
    outputOptions(output, "group2_pvalues", suspendWhenHidden = FALSE)


    return(list(
      comparison_results = comparison_results,
      group1_results = group1_results,
      group2_results = group2_results
    ))
  })
}

## To be copied in the UI
# mod_stats_calculations_ui("stats_calculations_1")

## To be copied in the server
# mod_stats_calculations_server("stats_calculations_1")
