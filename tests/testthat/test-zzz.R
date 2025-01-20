skip_on_cran()

test_that("test .onLoad", {
  if (testthat:::in_rcmd_check() || testthat:::in_covr()) {
    indep_session <- callr::r(function() {
      ordinalsimr_opts_preload <- grep("ordinalsimr.", names(options()), value = TRUE) |>
        purrr::set_names() |>
        purrr::map(~ getOption(.x))


      library(ordinalsimr)
      ordinalsimr:::.onLoad()

      ordinalsimr_opts_postload <- grep("ordinalsimr.", names(options()), value = TRUE) |>
        purrr::set_names() |>
        purrr::map(~ getOption(.x))

      return(
        list(
          ordinalsimr_opts_preload = ordinalsimr_opts_preload,
          ordinalsimr_opts_postload = ordinalsimr_opts_postload
        )
      )
    })



    # check that the options are empty on starting R
    expect_length(indep_session$ordinalsimr_opts_preload, 0)

    # check that options exist after loading the package
    expect_named(indep_session$ordinalsimr_opts_postload, c(
      "ordinalsimr.default_iterations",
      "ordinalsimr.default_ratio",
      "ordinalsimr.default_size_max",
      "ordinalsimr.default_size_min"
    ))
    expect_length(indep_session$ordinalsimr_opts_postload, 4)

    expect_equal(indep_session$ordinalsimr_opts_postload$ordinalsimr.default_iterations, 1000)
    expect_equal(indep_session$ordinalsimr_opts_postload$ordinalsimr.default_size_min, 30)
    expect_equal(indep_session$ordinalsimr_opts_postload$ordinalsimr.default_size_max, 200)
    expect_equal(indep_session$ordinalsimr_opts_postload$ordinalsimr.default_ratio, "50:50")
  } else if (!testthat:::in_rcmd_check()) {
    indep_session <- callr::r(function() {
      ordinalsimr_opts_preload <- grep("ordinalsimr.", names(options()), value = TRUE) |>
        purrr::set_names() |>
        purrr::map(~ getOption(.x))


      pkgload::load_all()
      ordinalsimr:::.onLoad()

      ordinalsimr_opts_postload <- grep("ordinalsimr.", names(options()), value = TRUE) |>
        purrr::set_names() |>
        purrr::map(~ getOption(.x))

      return(
        list(
          ordinalsimr_opts_preload = ordinalsimr_opts_preload,
          ordinalsimr_opts_postload = ordinalsimr_opts_postload
        )
      )
    })



    # check that the options are empty on starting R
    expect_length(indep_session$ordinalsimr_opts_preload, 0)

    # check that options exist after loading the package
    expect_named(indep_session$ordinalsimr_opts_postload, c(
      "ordinalsimr.default_iterations",
      "ordinalsimr.default_ratio",
      "ordinalsimr.default_size_max",
      "ordinalsimr.default_size_min"
    ))
    expect_length(indep_session$ordinalsimr_opts_postload, 4)

    expect_equal(indep_session$ordinalsimr_opts_postload$ordinalsimr.default_iterations, 1000)
    expect_equal(indep_session$ordinalsimr_opts_postload$ordinalsimr.default_size_min, 30)
    expect_equal(indep_session$ordinalsimr_opts_postload$ordinalsimr.default_size_max, 200)
    expect_equal(indep_session$ordinalsimr_opts_postload$ordinalsimr.default_ratio, "50:50")
  }
})
