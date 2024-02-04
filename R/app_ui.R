#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboardPlus::dashboardPage(

      shinydashboardPlus::dashboardHeader(),

      shinydashboardPlus::dashboardSidebar(
        sidebarMenu(
          menuItem("Simulation", tabName = "simulation_page", icon = icon("sliders")),
          menuItem("Distributions", tabName = "distributions_page", icon = icon("chart-simple")),
          menuItem("Report", tabName = "report_page", icon = icon("book"))
        )
      ),

      dashboardBody(
        tabItems(
          tabItem(tabName = "simulation_page",
                  fluidRow(
                    box(width = 3,
                        mod_iterations_ui("iterations_1"),
                        mod_sample_size_ui("sample_size_1"),
                        mod_sample_probabilities_ui("sample_probabilities_1")),
                    box(width = 9,
                        mod_data_entry_ui("data_entry_1"))
                  ),
                  fluidRow(
                    mod_stats_calculations_ui("stats_calculations_1")
                  )
          ),

          tabItem(tabName = "distributions_page",
                  mod_plot_distributions_ui("plot_distributions_1")
                  ),

          tabItem(tabName = "report_page",
                  mod_save_data_ui("save_data_1")
                  )

        )
      )


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
