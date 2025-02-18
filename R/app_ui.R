#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = "ordinalsimr",
      fillable = c("Simulation Inputs"),
      theme = bs_theme(5, "flatly"),
      navbar_options = navbar_options(class = "bg-primary", theme = "dark"),
      mod_homepage_ui("homepage_1"),
      mod_simulation_inputs_page_ui("simulation_inputs_page_1"),
      mod_distributions_page_ui("distributions_page_1"),
      mod_downloads_page_ui("downloads_page_1"),
      nav_spacer(),
      !!!mod_navbar_extras_ui("navbar_extras_1")
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
      app_title = "ordinalsimr"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
