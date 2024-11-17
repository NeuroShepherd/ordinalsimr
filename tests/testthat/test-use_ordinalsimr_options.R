# write tests for get_ordinalsimr_options() function

test_that("get_ordinalsimr_options() returns a list of ordinalsimr options", {
  # set options in a withr with_options environment
  withr::with_options(list(
    ordinalsimr.default_iterations = 500,
    ordinalsimr.default_size_min = 10,
    ordinalsimr.default_size_max = 100,
    ordinalsimr.default_ratio = "50:50",
    ordinalsimr.default_distributions = data.frame(c(0.4, 0.3, 0.3), c(0.8, 0.1, 0.1)),
    ordinalsimr.default_entry_rows = 5
  ), {
    options <- get_ordinalsimr_options()

    expect_true(is.list(options))
    expect_equal(length(options), 6)
    expect_equal(names(options), c(
      "ordinalsimr.default_iterations",
      "ordinalsimr.default_size_min",
      "ordinalsimr.default_size_max",
      "ordinalsimr.default_ratio",
      "ordinalsimr.default_distributions",
      "ordinalsimr.default_entry_rows"
    ))
    expect_equal(options$ordinalsimr.default_iterations, 500)
    expect_equal(options$ordinalsimr.default_size_min, 10)
    expect_equal(options$ordinalsimr.default_size_max, 100)
    expect_equal(options$ordinalsimr.default_ratio, "50:50")
    expect_equal(options$ordinalsimr.default_distributions, data.frame(c(0.4, 0.3, 0.3), c(0.8, 0.1, 0.1)))
    expect_equal(options$ordinalsimr.default_entry_rows, 5)
  })
})


# write tests for setting the ordinalsimr options with the set_ordinalsimr_options() function

test_that("check that set_ordinalsimr_options() works", {
  opts <- options()

  suppressMessages({
    # set options
    set_ordinalsimr_options(
      default_iterations = 1000,
      default_size_min = NULL,
      default_size_max = NULL,
      default_ratio = NULL,
      default_distributions = NULL,
      default_entry_rows = 10
    )

    # get options
    options <- get_ordinalsimr_options()


    # check options
    expect_true(is.list(options))
    expect_equal(length(options), 6)
    expect_equal(names(options), c(
      "ordinalsimr.default_iterations",
      "ordinalsimr.default_size_min",
      "ordinalsimr.default_size_max",
      "ordinalsimr.default_ratio",
      "ordinalsimr.default_distributions",
      "ordinalsimr.default_entry_rows"
    ))
    expect_equal(options$ordinalsimr.default_iterations, 1000)
    expect_equal(options$ordinalsimr.default_size_min, NULL)
    expect_equal(options$ordinalsimr.default_size_max, NULL)
    expect_equal(options$ordinalsimr.default_ratio, NULL)
    expect_equal(options$ordinalsimr.default_distributions, NULL)
    expect_equal(options$ordinalsimr.default_entry_rows, 10)


    # change the options to other values

    set_ordinalsimr_options(
      default_iterations = 500,
      default_size_min = 20,
      default_size_max = 200,
      default_ratio = "60:40",
      default_distributions = data.frame(c(0.5, 0.3, 0.2), c(0.7, 0.2, 0.1)),
      default_entry_rows = 15
    )

    # check the options again

    options <- get_ordinalsimr_options()

    expect_true(is.list(options))
    expect_equal(length(options), 6)

    expect_equal(options$ordinalsimr.default_iterations, 500)
    expect_equal(options$ordinalsimr.default_size_min, 20)
    expect_equal(options$ordinalsimr.default_size_max, 200)
    expect_equal(options$ordinalsimr.default_ratio, "60:40")
    expect_equal(options$ordinalsimr.default_distributions, data.frame(c(0.5, 0.3, 0.2), c(0.7, 0.2, 0.1)))
    expect_equal(options$ordinalsimr.default_entry_rows, 15)
  })


  options(opts)
})

# write tests for the helper function .set_options_helper()

test_that("check that .set_options_helper() works", {
  suppressMessages({
    opts <- options()


    .set_options_helper("ordinalsimr.default_iterations", 1000)
    expect_equal(getOption("ordinalsimr.default_iterations"), 1000)

    .set_options_helper("ordinalsimr.default_iterations", NULL)
    expect_equal(getOption("ordinalsimr.default_iterations"), NULL)

    expect_message(
      .set_options_helper(
        "ordinalsimr.default_size_min", 10,
        "The ordinalsimr.default_size_min option has been set to 10."
      )
    )


    options(opts)
  })

  expect_invisible(.set_options_helper(rvafve, 1000))
})
