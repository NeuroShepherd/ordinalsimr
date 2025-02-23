#' shinyapps_modal_notice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_shinyapps_modal_notice_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

#' shinyapps_modal_notice Server Functions
#'
#' @noRd
mod_shinyapps_modal_notice_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    if (getOption("is.shinyapps.deployment", default = FALSE)) {
      showModal(modalDialog(
        title = "ShinyApps.io Notice",
        markdown("**Welcome to the {ordinalsimr} package!**

                 This demo app is deployed on the free tier of ShinyApps.io, which means that **only 1GB of memory is provisioned to the app**. Running any simulations spawns 3 extra R instances, and this inevitably causes the memory to run out and the app will not function properly. My recommendation is to explore the app's features a bit here, but download the {ordinalsimr} package and run the simulations locally.

                 The package is free and open-source, and you can find it on <a href='https://cran.r-project.org/web/packages/ordinalsimr/index.html' target='_blank'>CRAN</a> or <a href='https://github.com/NeuroShepherd/ordinalsimr' target='_blank'>GitHub</a>.

                 **TL;DR: Visually explore the app here, but run simulations locally.**"),
        easyClose = TRUE,
        footer = modalButton("Acknoweldge"),
        size = "xl"
      ))
    }


  })
}

## To be copied in the UI
# mod_shinyapps_modal_notice_ui("shinyapps_modal_notice_1")

## To be copied in the server
# mod_shinyapps_modal_notice_server("shinyapps_modal_notice_1")
