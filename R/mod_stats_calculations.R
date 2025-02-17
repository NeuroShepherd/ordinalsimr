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
                                          run_simulation_button, t1_error_toggle, kill_button, reactive_bg_process) {
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

      run_simulation_wrapper <- function(sample_size, sample_prob, prob0, prob1, niter, included,
                                         .rng_kind, .rng_normal_kind, .rng_sample_kind) {
        ordinalsimr::run_simulations(sample_size, sample_prob = sample_prob, prob0 = prob0, prob1 = prob1, niter = niter, included = included,
                                     .rng_kind = .rng_kind, .rng_normal_kind = .rng_normal_kind, .rng_sample_kind = .rng_sample_kind)
      }


      tmepte <- callr::r_bg(run_simulation_wrapper,
                            args = list(sample_size = sample_size, sample_prob = sample_prob, prob0 = prob0,
                                        prob1 = prob1, niter = niter, included = included,
                                        .rng_kind = .rng_kind, .rng_normal_kind = .rng_normal_kind, .rng_sample_kind = .rng_sample_kind))


    }



    # NOTE: the whole list is reactive, and need to subset elements after
    # calling reactivity
    # usage example: parameters()$null_probs

    observeEvent(run_simulation_button(), {
      reactive_bg_process$bg_cancelled <- FALSE
      reactive_bg_process$bg_process_comparison <- background_process(
        parameters()$sample_size,
        parameters()$sample_prob,
        parameters()$prob0,
        parameters()$prob1,
        parameters()$iterations,
        parameters()$included_tests,
        .rng_kind = rng_info$rng_kind(),
        .rng_normal_kind = rng_info$rng_normal_kind(),
        .rng_sample_kind = rng_info$rng_sample_kind()
        )
      reactive_bg_process$bg_started <- TRUE
      reactive_bg_process$bg_running <- TRUE

    }, ignoreInit = TRUE)

    observeEvent(kill_button(), {
      cat(paste("Killing process - PID:", reactive_bg_process$bg_process_comparison$get_pid(), "\n"))
      reactive_bg_process$bg_process_comparison$kill()
      reactive_bg_process$bg_process_group1$kill()
      reactive_bg_process$bg_process_group2$kill()
      reactive_bg_process$bg_cancelled <- TRUE
    })


    comparison_results <- reactive({
      req(reactive_bg_process$bg_started)

      if (reactive_bg_process$bg_process_comparison$is_alive()) {
        invalidateLater(millis = 3000, session = session)
      } else {
        reactive_bg_process$bg_running <- FALSE
        reactive_bg_process$bg_process_comparison$get_result()
        }
    })






    observeEvent(run_simulation_button(), {
      reactive_bg_process$bg_process_group1 <- background_process(
        parameters()$sample_size,
        parameters()$sample_prob,
        parameters()$prob0,
        parameters()$prob0,
        parameters()$iterations,
        parameters()$included_tests,
        .rng_kind = rng_info$rng_kind(),
        .rng_normal_kind = rng_info$rng_normal_kind(),
        .rng_sample_kind = rng_info$rng_sample_kind()
      )
    }, ignoreInit = TRUE)


    group1_results <- reactive({
      req(reactive_bg_process$bg_started)
      req(parameters()$t1_error_toggle %in% c("both", "group1"))

      if (reactive_bg_process$bg_process_group1$is_alive()) {
        invalidateLater(millis = 3000, session = session)
      } else {
        reactive_bg_process$bg_process_group1$get_result()
      }
    })


    observeEvent(run_simulation_button(), {
      reactive_bg_process$bg_process_group2 <- background_process(
        parameters()$sample_size,
        parameters()$sample_prob,
        parameters()$prob1,
        parameters()$prob1,
        parameters()$iterations,
        parameters()$included_tests,
        .rng_kind = rng_info$rng_kind(),
        .rng_normal_kind = rng_info$rng_normal_kind(),
        .rng_sample_kind = rng_info$rng_sample_kind()
      )
    }, ignoreInit = TRUE)

    group2_results <- reactive({
      req(reactive_bg_process$bg_started)
      req(parameters()$t1_error_toggle %in% c("both", "group2"))

      if (reactive_bg_process$bg_process_group2$is_alive()) {
        invalidateLater(millis = 3000, session = session)
      } else {
        reactive_bg_process$bg_process_group2$get_result()
      }
    })




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
