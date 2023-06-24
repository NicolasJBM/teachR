#' @name course_test_ui
#' @title Select and load a test
#' @author Nicolas Mangin
#' @description Module facilitating the selection of tests and tests to edit and work with.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return A list co ntaining a test and a test folder path.
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny NS
#' @importFrom shiny selectInput
#' @importFrom shiny span
#' @export


course_test_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        8,
        shiny::uiOutput(ns("slcttest"))
      ),
      shiny::column(
        4,
        shiny::actionButton(
          ns("newtest"),
          shiny::span("New test", title = ""),
          icon = shiny::icon("wand-magic-sparkles"),
          style = "background-color:#000066;color:#FFF; width:100%;margin-top:25px;"
        )
      )
    )
  )
}

