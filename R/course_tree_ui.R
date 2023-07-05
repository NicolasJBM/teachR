#' @name course_tree_ui
#' @title Select and load a tree
#' @author Nicolas Mangin
#' @description Module facilitating the selection of trees and tests to edit and work with.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return A list co ntaining a tree and a test folder path.
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny NS
#' @importFrom shiny selectInput
#' @importFrom shiny span
#' @export


course_tree_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        8,
        shiny::uiOutput(ns("treepattern"))
      ),
      shiny::column(
        4,
        shiny::actionButton(
          ns("updatetree"),
          shiny::span("Update tree", title = ""),
          icon = shiny::icon("arrows-rotate"),
          style = "background-color:#006699;color:#FFF; width:100%;margin-top:25px;"
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        8,
        shiny::uiOutput(ns("slcttree"))
      ),
      shiny::column(
        4,
        shiny::actionButton(
          ns("newtree"),
          shiny::span("New tree", title = ""),
          icon = shiny::icon("wand-magic-sparkles"),
          style = "background-color:#000066;color:#FFF; width:100%;margin-top:25px;"
        )
      )
    )
  )
}

