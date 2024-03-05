#' @name statistics_update_ui
#' @title Update documents and data
#' @author Nicolas Mangin
#' @description Module facilitating the update of underlying course documents, trees, data, and statistics.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny span
#' @export


statistics_update_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::actionButton(
    ns("updatestatistics"),
    shiny::span("Update statistics", title = "Count observations, compute basic statistics as well as Item Response Theory statistics for each document and item."),
    icon = shiny::icon("rotate"),
    style = "background-color:#006666;color:#FFF;width:100%;margin-top:10px;margin-bottom:10px;border:0px;"
  )
}

