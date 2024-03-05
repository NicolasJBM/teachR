#' @name course_test_ui
#' @title Select and load a test
#' @author Nicolas Mangin
#' @description Module facilitating the selection of a test to edit, grade, and feedback.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return A list containing a test and a test folder path used in testR, gradR, and reportR.
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny span
#' @importFrom shiny uiOutput
#' @export


course_test_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::actionButton(
      ns("newtest"),
      shiny::span("New test", title = ""),
      icon = shiny::icon("wand-magic-sparkles"),
      style = "background-color:#003366;color:#FFF;width:100%;margin-top:10px;margin-bottom:10px;border:0px;"
    ),
    shiny::uiOutput(ns("testpattern")),
    shiny::uiOutput(ns("slcttest"))
  )
}

