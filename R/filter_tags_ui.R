#' @name filter_tags_ui
#' @title Select documents from tags (UI)
#' @author Nicolas Mangin
#' @description Module allowing the user to select documents by clicking on the different sections of a classification tags.
#' @param id ID of the module to connect the user interface to the appropriate server side.
#' @return Tibble. List of selected documents.
#' @importFrom shiny NS
#' @importFrom shiny textOutput
#' @importFrom shiny uiOutput
#' @export

filter_tags_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::uiOutput(ns("commontags_filters")),
    shiny::uiOutput(ns("customtags_filters"))
  )
}

