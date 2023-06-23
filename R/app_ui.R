#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      # h1("SyntheticParameters")
      title = "Synthetic Paremeters",

      mod_data_entry_ui("data_entry_1"),
      mod_test_pass_data_ui("test_pass_data_1"),
      mod_stats_calculations_ui("stats_calculations_1")


    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SyntheticParameters"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
