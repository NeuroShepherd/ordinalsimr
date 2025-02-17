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

  list(
    nav_panel(
      "Comparison p-values",
      shinycssloaders::withSpinner(DT::dataTableOutput(ns("results_table")), type = 8)
    ),
    nav_panel(
      "Group 1 p-values",
      shinycssloaders::withSpinner(DT::dataTableOutput(ns("group1_pvalues")), type = 8)
    ),
    nav_panel(
      "Group 2 p-values",
      shinycssloaders::withSpinner(DT::dataTableOutput(ns("group2_pvalues")), type = 8)
    )
  )
}

#' stats_calculations Server Functions
#'
#' @noRd
mod_stats_calculations_server <- function(id, probability_data, sample_prob, iterations, sample_size, rng_info, included_tests,
                                          run_simulation_button, t1_error_toggle, kill_button) {
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
        included_tests = included_tests(),
        t1_error_toggle = t1_error_toggle()
      )
    })



    background_process <- function(sample_size, sample_prob, prob0, prob1, niter, included = "all",
                                   .rng_kind = NULL, .rng_normal_kind = NULL, .rng_sample_kind = NULL) {

      run_simulation_wrapper <- function(sample_size, sample_prob, prob0, prob1, niter, included = "all",
                                         .rng_kind = NULL, .rng_normal_kind = NULL, .rng_sample_kind = NULL) {
        ordinalsimr::run_simulations(sample_size, sample_prob = sample_prob, prob0 = prob0, prob1 = prob1, niter = niter, included = included,
                                     .rng_kind = .rng_kind, .rng_normal_kind = .rng_normal_kind, .rng_sample_kind = .rng_sample_kind)
      }


      tmepte <- callr::r_bg(run_simulation_wrapper,
                            args = list(sample_size = 100:500, sample_prob = c(0.5, 0.5), prob0 = c(0.5, 0.5),
                                        prob1 = c(0.5, 0.5), niter = 100, included = "all"))


    }


    empty_table <- data.frame(matrix(ncol = 13))
    colnames(empty_table) <- c('Wilcoxon', 'Fisher', 'Chi Squared (No Correction)', 'Chi Squared (Correction)', 'Prop. Odds', 'Coin Indep. Test', 'run', 'y', 'x', 'n_null', 'n_intervene', 'sample_size', 'K')


    reactive_bg_process <- reactiveValues(bg_process = NULL,
                                          empty_table = empty_table)


    # NOTE: the whole list is reactive, and need to subset elements after
    # calling reactivity
    # usage example: parameters()$null_probs

    intermediate_results <- eventReactive(run_simulation_button(), {
      background_process()
    })

    comparison_results <- reactive({

      req(intermediate_results())

      if (intermediate_results()$is_alive()) {
        invalidateLater(millis = 3000, session = session)
        reactive_bg_process$empty_table
      } else {
        intermediate_results()$get_result()
        }
    })


    observeEvent(kill_button(), {
      cat(paste("Killing process - PID:", intermediate_results()$get_pid(), "\n"))
      intermediate_results()$kill()
    })

    # comparison_results <- reactive({
    #   invalidateLater(millis = 1000, session = session)
    #
    #   if (reactive_bg_process$bg_process()$is_alive()) {
    #     reactive_bg_process$empty_table()
    #   } else {
    #     reactive_bg_process$bg_process()$get_result()}
    # })


    # observe({
    #   invalidateLater(1000)
    #   req(reactive_bg_process$bg_process)
    #   if(reactive_bg_process$bg_process$poll_io(0)[["process"]] == "ready") {
    #     print(reactive_bg_process$bg_process$get_result())
    #     reactive_bg_process$bg_process <- NULL
    #   }
    # })






    group1_results <- eventReactive(run_simulation_button(), {
      if (parameters()$t1_error_toggle %in% c("both", "group1")) {
        withProgress(message = "Group 1:", value = 0, {
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
        })
      }
    })

    group2_results <- eventReactive(run_simulation_button(), {
      if (parameters()$t1_error_toggle %in% c("both", "group2")) {
        withProgress(message = "Group 2:", value = 0, {
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
        })
      }
    })


    output$results_table <- DT::renderDataTable({
      validate(
        need(comparison_results(), "No results yet or simulation killed.")
      )
      comp_res <- comparison_results() %>%
        bind_rows() %>%
        dplyr::select(
          .data$sample_size,
          dplyr::any_of(c(
            "Wilcoxon", "Fisher", "Chi Squared (No Correction)",
            "Chi Squared (Correction)", "Prop. Odds", "Coin Indep. Test"
          ))
        )

      comp_res %>%
        DT::datatable(options = list(scrollX = TRUE)) %>%
        DT::formatRound(2:ncol(comp_res), digits = 5)
    })
    outputOptions(output, "results_table", suspendWhenHidden = FALSE)

    # if not keeping these output tables, use observe({group1_results()}) to
    # ensure evaluation
    output$group1_pvalues <- DT::renderDataTable({
      validate(
        need(group1_results(), "No simulations run for Group 1.")
      )

      g1_res <- group1_results() %>%
        bind_rows() %>%
        dplyr::select(
          .data$sample_size,
          dplyr::any_of(c(
            "Wilcoxon", "Fisher", "Chi Squared (No Correction)",
            "Chi Squared (Correction)", "Prop. Odds", "Coin Indep. Test"
          ))
        )

      g1_res %>%
        DT::datatable(options = list(scrollX = TRUE)) %>%
        DT::formatRound(2:ncol(g1_res), digits = 5)
    })
    outputOptions(output, "group1_pvalues", suspendWhenHidden = FALSE)

    output$group2_pvalues <- DT::renderDataTable({
      validate(
        need(group2_results(), "No simulations run for Group 2.")
      )

      g2_res <- group2_results() %>%
        bind_rows() %>%
        dplyr::select(
          .data$sample_size,
          dplyr::any_of(c(
            "Wilcoxon", "Fisher", "Chi Squared (No Correction)",
            "Chi Squared (Correction)", "Prop. Odds", "Coin Indep. Test"
          ))
        )

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
