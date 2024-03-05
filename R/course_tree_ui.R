#' @name course_tree_ui
#' @title Select and load a tree
#' @author Nicolas Mangin
#' @description Module facilitating the loading of a classification tree.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return A list containing the tree as a table and a json object
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny span
#' @importFrom shiny uiOutput
#' @export


course_tree_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::actionButton(
      ns("newtree"),
      shiny::span("New tree", title = ""),
      icon = shiny::icon("wand-magic-sparkles"),
      style = "background-color:#003366;color:#FFF;width:100%;margin-top:10px;margin-bottom:10px;border:0px;"
    ),
    shiny::uiOutput(ns("treepattern")),
    shiny::uiOutput(ns("slcttree"))
  )
}

