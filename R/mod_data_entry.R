#' data_entry UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_entry_ui <- function(id){
  ns <- NS(id)

  tagList(
   rhandsontable::rHandsontableOutput(ns("hottable"))
   )

}

#' data_entry Server Functions
#'
#' @noRd
mod_data_entry_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    values = reactiveValues()

    probability_data = reactive({

      if (!is.null(input$hottable)) {
        entered_data = rhandsontable::hot_to_r(input$hottable)
      } else {
        if (is.null(values[["entered_data"]])) {


          # create a placehodler for generating the number of rows in the data table
          # which will be based on the number of possible outcomes
          # table_row_number <- input$number_of_outcomes

          rows <- 6
          entered_data = data.frame(`Null Group Probabilities` = rep(0,rows),
                          `Intervention Group Probs.` = rep(0,rows),
                          check.names = FALSE)
        } else
          entered_data = values[["entered_data"]]
      }

      values[["entered_data"]] = entered_data
      entered_data

    })

    output$hottable <- rhandsontable::renderRHandsontable({

      entered_data = probability_data()

      # https://stackoverflow.com/questions/58746194/shiny-and-rhandsontable-conditional-cell-column-formatting-based-on-column-sum
      # not sure why I need to start at 1 and decrement from there, but it works...
      col_highlight_1 <- 1 - unname(which(colSums(entered_data[c(1)]) == 1))

      col_highlight_2 <- unname(which(colSums(entered_data[c(2)]) == 1))


      if (!is.null(entered_data)) {
        entered_data %>%
          rhandsontable::rhandsontable(stretchH = "all",
                                       col_highlight_1 = col_highlight_1,
                                       col_highlight_2 = col_highlight_2) %>%
          rhandsontable::hot_col("Null Group Probabilities", format = "0.00000") %>%
          rhandsontable::hot_col("Intervention Group Probs.", format = "0.00000") %>%
          rhandsontable::hot_cols(.,
            renderer = "function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.NumericRenderer.apply(this, arguments);


              if (instance.params) {
                    hcols_1 = instance.params.col_highlight_1;
                    hcols_1 = hcols_1 instanceof Array ? hcols_1 : [hcols_1];

                    hcols_2 = instance.params.col_highlight_2;
                    hcols_2 = hcols_2 instanceof Array ? hcols_2 : [hcols_2];
                }


              if (instance.params && hcols_1.includes(col)) {
                    td.style.background = '#aef0a8';
              }
              if (instance.params && hcols_2.includes(col)) {
                    td.style.background = '#aef0a8';
              }
                return td;
          }")
        }

    })

    return(probability_data)


  })
}

## To be copied in the UI
# mod_data_entry_ui("data_entry_1")

## To be copied in the server
# mod_data_entry_server("data_entry_1")
