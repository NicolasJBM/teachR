#' @name filter_languages_ui
#' @title Filter documents based on languages
#' @author Nicolas Mangin
#' @description Module allowing the user to find select document based on whether they exist in other languages.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Tibble. list of documents existing in all the selected languages.
#' @importFrom shiny NS
#' @importFrom shiny uiOutput
#' @export


filter_languages_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::uiOutput(ns("filtexistinglang")),
    shiny::uiOutput(ns("filtmissinglang"))
  )
}

