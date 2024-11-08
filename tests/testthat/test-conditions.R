
# skip_on_covr()

test_that("figure out how to do conditions right", {


  is_chk <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") == .packageName
  not_cran <- Sys.getenv("NOT_CRAN") == "true"

  print(paste(".packageName is", .packageName))
  print(paste("_R_CHECK_PACKAGE_NAME_ is", Sys.getenv("_R_CHECK_PACKAGE_NAME_", "")))
  print(paste("NOT_CRAN is", Sys.getenv("NOT_CRAN")))
  print(paste("is_chk is", is_chk))
  print(paste("not_cran is", not_cran))


  if (is_chk) {
    print("using library()")
    library("ordinalsimr")
  } else if (!is_chk) {
    print("using load_all()")
    pkgload::load_all()
  }



})
