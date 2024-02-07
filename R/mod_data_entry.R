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
   rhandsontable::rHandsontableOutput(ns("hottable")),
   br(),
   actionButton(ns("add_row"), "Add Row"),
   actionButton(ns("delete_row"), "Delete Row")
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

          entered_data = data.frame(`Group 1 Probabilities` = rep(0,3),
                          `Group 2 Probabilities` = rep(0,3),
                          check.names = FALSE)

        } else
          entered_data = values[["entered_data"]]
      }

      values[["entered_data"]] = entered_data
      entered_data

    })



    output$hottable <- rhandsontable::renderRHandsontable({

      entered_data_2 = probability_data()

      # https://stackoverflow.com/questions/58746194/shiny-and-rhandsontable-conditional-cell-column-formatting-based-on-column-sum
      # not sure why I need to start at 1 and decrement from there, but it works...
      col_highlight_1 <- entered_data_2[c(1)] %>%
        colSums() %>%
        dplyr::near(., 1) %>%
        which() %>%
        unname() %>%
        {1 - .}

      col_highlight_2 <- entered_data_2[c(2)] %>%
        colSums() %>%
        dplyr::near(., 1) %>%
        which() %>%
        unname()

      if (!is.null(entered_data_2)) {
        entered_data_2 %>%
          rhandsontable::rhandsontable(stretchH = "all",
                                       col_highlight_1 = col_highlight_1,
                                       col_highlight_2 = col_highlight_2) %>%
          rhandsontable::hot_col("Group 1 Probabilities", format = "0.00000") %>%
          rhandsontable::hot_col("Group 2 Probabilities", format = "0.00000") %>%
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








# Replace rhandsontable with DT. Use these links below for a reactive, editable
# table and for info on add/remove rows
# https://thatdatatho.com/r-shiny-data-table-proxy-replace-data/
# https://community.rstudio.com/t/how-do-i-create-an-editable-table-to-allow-user-input-draw-scatterplot-and-fit-a-curve-through-those-points/83802/2
# https://www.google.com/search?q=r+datatable+data+entry+shiny&client=firefox-b-d&sca_esv=602165150&ei=CXe2ZYDcKauoxc8PtKG62Ac&ved=0ahUKEwjAhP6Ot4CEAxUrVPEDHbSQDnsQ4dUDCBA&uact=5&oq=r+datatable+data+entry+shiny&gs_lp=Egxnd3Mtd2l6LXNlcnAiHHIgZGF0YXRhYmxlIGRhdGEgZW50cnkgc2hpbnkyBRAhGKABMgUQIRigATIFECEYoAFIoQZQjwFYgAVwAXgBkAEAmAGVAaABvgWqAQMxLjW4AQPIAQD4AQHCAgoQABhHGNYEGLAD4gMEGAAgQYgGAZAGCA&sclient=gws-wiz-serp
