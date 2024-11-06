#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom utils zip
## usethis namespace: end
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
