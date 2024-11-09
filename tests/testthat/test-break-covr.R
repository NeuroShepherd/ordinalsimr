

test_that("test R_COVR env var", {

  is_chk <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") == .packageName
  not_cran <- Sys.getenv("NOT_CRAN") == "true"



  if (testthat:::in_covr()) {

    callr::r(function() {

      print("using load_all()")
      pkgload::load_all()

      print("using library()")
      library(ordinalsimr)

    })
    }


})
