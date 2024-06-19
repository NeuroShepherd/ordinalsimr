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

      shinydashboardPlus::dashboardHeader(
        title = "ordinalsimr",
        tags$li(class = "dropdown", tags$a(href = "https://neuroshepherd.github.io/ordinalsimr/",
                                           icon("book-open-reader"), "Docs", target = "_blank")),
        tags$li(class = "dropdown", tags$a(href = "https://github.com/NeuroShepherd/ordinalsimr/issues",
                                           icon("circle-exclamation"), "Issues", target = "_blank")),
        tags$li(class = "dropdown", tags$a(href = "https://github.com/NeuroShepherd/ordinalsimr",
                                           icon("github"), "GitHub", target = "_blank"))
      ),

      shinydashboardPlus::dashboardSidebar(
        sidebarMenu(
          menuItem("Home", tabName = "homeinfo_page", icon = icon("book")),
          menuItem("Simulation", tabName = "simulation_page", icon = icon("sliders")),
          menuItem("Distributions", tabName = "distributions_page", icon = icon("chart-simple")),
          menuItem("Report", tabName = "report_page", icon = icon("markdown")),
          menuItem("Data Download", tabName = "download_page", icon = icon("file-excel"))
        )
      ),

      dashboardBody(
        tabItems(
          tabItem(tabName = "homeinfo_page",
                  mod_homepage_ui("homepage_1")
                  ),
          tabItem(tabName = "simulation_page",
                  fluidRow(
                    box(width = 3,
                        mod_iterations_ui("iterations_1"),
                        mod_sample_size_ui("sample_size_1"),
                        mod_sample_probabilities_ui("sample_probabilities_1"),
                        mod_rng_option_ui("rng_option_1")),
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
                  mod_report_generator_ui("report_generator_1")
          ),

          tabItem(tabName = "download_page",
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
      app_title = "ordinalsimr"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
