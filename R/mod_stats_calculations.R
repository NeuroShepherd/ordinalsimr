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
    comparison_progress = list(
      tags$b("Comparison Progress"), br(),
      shinyWidgets::progressBar(
        ns("comparison_progress"),
        value = 0,
        display_pct = TRUE
      )
      ),
    group1_progress = uiOutput(ns("group1_progress_ui")),
    group2_progress = uiOutput(ns("group2_progress_ui"))
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



    # Kill all 3 background processes on push
    observeEvent(kill_button(), {
      try(cat(paste("Killing process - PID:", reactive_bg_process$bg_process_comparison$get_pid(), "\n")), silent = TRUE)
      try(cat(paste("Killing process - PID:", reactive_bg_process$bg_process_group1$get_pid(), "\n")), silent = TRUE)
      try(cat(paste("Killing process - PID:", reactive_bg_process$bg_process_group2$get_pid(), "\n")), silent = TRUE)
      try(reactive_bg_process$bg_process_comparison$kill(), silent = TRUE)
      try(reactive_bg_process$bg_process_group1$kill(), silent = TRUE)
      try(reactive_bg_process$bg_process_group2$kill(), silent = TRUE)
      reactive_bg_process$bg_cancelled <- TRUE
    })


    # NOTE: the whole list is reactive, and need to subset elements after
    # calling reactivity
    # usage example: parameters()$null_probs

    ### COMPARISONS

    # Start the comparison processing
    observeEvent(run_simulation_button(), {
      reactive_bg_process$bg_cancelled <- FALSE
      reactive_bg_process$comparison_output_tracker_file <- tempfile(fileext = ".txt")
      reactive_bg_process$bg_process_comparison <- run_simulations_in_background(
        parameters()$sample_size,
        parameters()$sample_prob,
        parameters()$prob0,
        parameters()$prob1,
        parameters()$iterations,
        parameters()$included_tests,
        .rng_kind = rng_info$rng_kind(),
        .rng_normal_kind = rng_info$rng_normal_kind(),
        .rng_sample_kind = rng_info$rng_sample_kind(),
        tempfile = reactive_bg_process$comparison_output_tracker_file
        )
      reactive_bg_process$bg_process_comparison_started <- TRUE
      reactive_bg_process$bg_running_comparisons <- TRUE

    }, ignoreInit = TRUE)


    # Wait for and eventually return the comparison results
    comparison_results <- reactive({
      req(reactive_bg_process$bg_process_comparison_started)
      if (reactive_bg_process$bg_process_comparison$is_alive()) {
        invalidateLater(millis = 3000, session = session)
      } else {
        reactive_bg_process$bg_running_comparisons <- FALSE
        reactive_bg_process$bg_process_comparison$get_result()
      }
    })


    # Check progress and update the progress bar
    observe({
      req(reactive_bg_process$bg_process_comparison_started)
      # req(reactive_bg_process$comparison_output_tracker_file)
      invalidateLater(100, session)
      if (file.exists(reactive_bg_process$comparison_output_tracker_file) &&
          !is.null(reactive_bg_process$comparison_output_tracker_file)) {
        shinyWidgets::updateProgressBar(
          session = session,
          id = "comparison_progress",
          value = 1 + as.numeric(readLines(reactive_bg_process$comparison_output_tracker_file)) - min(parameters()$sample_size),
          total = 1 + max(parameters()$sample_size) - min(parameters()$sample_size)
          )
      }
    })






    ### GROUP 1

    observeEvent(run_simulation_button(), {
      reactive_bg_process$group1_output_tracker_file <- tempfile(fileext = ".txt")
      reactive_bg_process$bg_process_group1 <- run_simulations_in_background(
        parameters()$sample_size,
        parameters()$sample_prob,
        parameters()$prob0,
        parameters()$prob0,
        parameters()$iterations,
        parameters()$included_tests,
        .rng_kind = rng_info$rng_kind(),
        .rng_normal_kind = rng_info$rng_normal_kind(),
        .rng_sample_kind = rng_info$rng_sample_kind(),
        tempfile = reactive_bg_process$group1_output_tracker_file
      )
      reactive_bg_process$bg_process_group1_started <- TRUE
      reactive_bg_process$bg_running_group1 <- TRUE
    }, ignoreInit = TRUE)


    group1_results <- reactive({
      req(reactive_bg_process$bg_process_group1_started)
      req(parameters()$t1_error_toggle %in% c("both", "group1"))

      if (reactive_bg_process$bg_process_group1$is_alive()) {
        invalidateLater(millis = 3000, session = session)
      } else {
        reactive_bg_process$bg_running_group1 <- FALSE
        reactive_bg_process$bg_process_group1$get_result()
      }
    })


    observe({
      req(reactive_bg_process$bg_process_group1_started)
      req(reactive_bg_process$group1_output_tracker_file)
      req(parameters()$t1_error_toggle %in% c("both", "group1"))
      invalidateLater(100, session)
      if (file.exists(reactive_bg_process$group1_output_tracker_file) &&
          !is.null(reactive_bg_process$group1_output_tracker_file)) {
        shinyWidgets::updateProgressBar(
          session = session,
          id = "group1_progress",
          value = 1 + as.numeric(readLines(reactive_bg_process$group1_output_tracker_file)) - min(parameters()$sample_size),
          total = 1 + max(parameters()$sample_size) - min(parameters()$sample_size)
        )
      }
    })




    #### GROUP 2

    observeEvent(run_simulation_button(), {
      reactive_bg_process$group2_output_tracker_file <- tempfile(fileext = ".txt")
      reactive_bg_process$bg_process_group2 <- run_simulations_in_background(
        parameters()$sample_size,
        parameters()$sample_prob,
        parameters()$prob1,
        parameters()$prob1,
        parameters()$iterations,
        parameters()$included_tests,
        .rng_kind = rng_info$rng_kind(),
        .rng_normal_kind = rng_info$rng_normal_kind(),
        .rng_sample_kind = rng_info$rng_sample_kind(),
        tempfile = reactive_bg_process$group2_output_tracker_file
      )
      reactive_bg_process$bg_process_group2_started <- TRUE
      reactive_bg_process$bg_running_group2 <- TRUE
    }, ignoreInit = TRUE)

    group2_results <- reactive({
      req(reactive_bg_process$bg_process_group2_started)
      req(parameters()$t1_error_toggle %in% c("both", "group2"))

      if (reactive_bg_process$bg_process_group2$is_alive()) {
        invalidateLater(millis = 3000, session = session)
      } else {
        reactive_bg_process$bg_running_group2 <- FALSE
        reactive_bg_process$bg_process_group2$get_result()
      }
    })


    observe({
      req(reactive_bg_process$bg_process_group2_started)
      req(reactive_bg_process$group2_output_tracker_file)
      req(parameters()$t1_error_toggle %in% c("both", "group2"))
      invalidateLater(100, session)
      if (file.exists(reactive_bg_process$group2_output_tracker_file) &&
          !is.null(reactive_bg_process$group2_output_tracker_file)) {
        shinyWidgets::updateProgressBar(
          session = session,
          id = "group2_progress",
          value = 1 + as.numeric(readLines(reactive_bg_process$group2_output_tracker_file)) - min(parameters()$sample_size),
          total = 1 + max(parameters()$sample_size) - min(parameters()$sample_size)
        )
      }
    })



    output$group1_progress_ui <- renderUI({
      req(parameters()$t1_error_toggle %in% c("both", "group1"))
      list(
        tags$b("Group 1 Progress"), br(),
        shinyWidgets::progressBar(
          ns("group1_progress"),
          value = 0,
          display_pct = TRUE
        )
      )
    })

    output$group2_progress_ui <- renderUI({
      req(parameters()$t1_error_toggle %in% c("both", "group2"))
      list(
        tags$b("Group 2 Progress"), br(),
        shinyWidgets::progressBar(
          ns("group2_progress"),
          value = 0,
          display_pct = TRUE
        )
      )
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
