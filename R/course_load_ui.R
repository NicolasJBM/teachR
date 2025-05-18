#' @name course_load_ui
#' @title Load course data
#' @author Nicolas Mangin
#' @description Module facilitating the loading of all relevant course data.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return A list of course data.
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny icon
#' @export


course_load_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::actionButton(
      ns("loadcourse"), "Load course", icon = shiny::icon("upload"),
      style = "background-color:#003366;color:#FFF;width:150px;border:0px;margin-left:10px;"
    ),
    shiny::actionButton(
      ns("updatecourse"), "Update course", icon = shiny::icon("chalkboard"),
      style = "background-color:#009999;color:#FFF;width:150px;border:0px;margin-left:10px;"
    ),
    shiny::actionButton(
      ns("updatepackage"), "Update package", icon = shiny::icon("box-archive"),
      style = "background-color:#006666;color:#FFF;width:150px;border:0px;margin-left:10px;"
    ),
    shiny::actionButton(
      ns("updateanalytics"), "Update analytics", icon = shiny::icon("magnifying-glass-chart"),
      style = "background-color:#003333;color:#FFF;width:150px;border:0px;margin-left:10px;"
    )
  )
}

