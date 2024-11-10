

test_that("test R_COVR env var", {

  is_chk <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") == .packageName
  not_cran <- Sys.getenv("NOT_CRAN") == "true"



  if (testthat:::in_covr()) {

    opts <- callr::r(function() {

      ordinalsimr_opts_preload <- grep("ordinalsimr.", names(options()), value = TRUE) |>
        purrr::set_names() |>
        purrr::map(~getOption(.x))

      print("using load_all()")
      pkgload::load_all()

      ordinalsimr_opts_postload <- grep("ordinalsimr.", names(options()), value = TRUE) |>
        purrr::set_names() |>
        purrr::map(~getOption(.x))

      return(
        list(ordinalsimr_opts_preload = ordinalsimr_opts_preload,
             ordinalsimr_opts_postload = ordinalsimr_opts_postload))


    })

  }

  print(opts)
  expect_length(opts$ordinalsimr_opts_preload, 0)





})
