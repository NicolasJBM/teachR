#' @name filter_tree_ui
#' @title Select documents from tree (UI)
#' @author Nicolas Mangin
#' @description Module allowing the user to select documents by clicking on the different sections of a classification tree.
#' @param id ID of the module to connect the user interface to the appropriate server side.
#' @return Tibble. List of selected documents.
#' @importFrom jsTreeR jstreeOutput
#' @importFrom shiny NS
#' @importFrom shiny textOutput
#' @export

filter_tree_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    jsTreeR::jstreeOutput(ns("selection_tree"))
  )
}

