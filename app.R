# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
# library(pkgload)
# pkgload::load_all(export_all = TRUE,helpers = FALSE,attach_testthat = FALSE)
devtools::install(quick = TRUE, dependencies = FALSE, upgrade = FALSE)
options( "golem.app.prod" = TRUE, "is.shinyapps.deployment" = TRUE)
ordinalsimr::run_app() # add parameters here (if any)
