

# use callr package to test the behavvior of .onLoad
# https://forum.posit.co/t/executing-onload-in-different-test-instances/181269/2

test_that("test .onLoad", {

    callr::r(function() {
      loadNamespace("ordinalsimr")
    })

})
