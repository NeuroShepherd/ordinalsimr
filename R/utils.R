
#' Parse Ratio Text
#'
#' This function parses text from ratios which are written in the format of 1-2 digit numbers separated by a colon and trailing with another 1-2 digit number. The text is processed into a numeric vector of length 2 containing the two numbers.
#'
#' @param text A string of in the form of e.g. 5:95 or 70:30
#'
#' @return Numeric vector of length 2
#' @export
#'
#' @examples
#'
#' parse_ratio_text("70:30")
#'
parse_ratio_text <- function(text) {

  assert_that(str_detect(text, "[[:digit:]]{1,2}:[[:digit:]]{1,2}"),
              msg = "Incorrect ratio format.")

  pre_value <- as.numeric(str_extract(text, "^[[:digit:]]{1,2}"))
  post_value <- as.numeric(str_extract(text, "[[:digit:]]{1,2}$"))

  assert_that(pre_value + post_value == 100,
              msg = "Ratio does not sum to 100.")

  c(pre_value/100, post_value/100)

}

