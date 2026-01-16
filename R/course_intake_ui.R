#' @name course_intake_ui
#' @title Select and load a intake
#' @author Nicolas Mangin
#' @description Module facilitating the loading of a classification intake.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return A list containing the intake as a table and a json object
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny span
#' @importFrom shiny uiOutput
#' @importFrom rhandsontable rHandsontableOutput
#' @export


course_intake_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shinydashboardPlus::box(
      title = "Definition",
      status = "navy",
      solidHeader = TRUE,
      width = 3,
      collapsible = TRUE,
      collapsed = FALSE,
      closable = FALSE,
      icon = shiny::icon("id-card"),
      shiny::actionButton(
        ns("newintake"),
        shiny::span("New intake", title = ""),
        icon = shiny::icon("wand-magic-sparkles"),
        style = "background-color:#003366;color:#FFF;width:100%;margin-top:10px;margin-bottom:10px;border:0px;"
      ),
      shiny::tags$hr(),
      shiny::actionButton(
        ns("saveintake"), "Save intake", icon = shiny::icon("floppy-disk"),
        style = "background-color:#006600;color:#FFF;
          width:100%;margin-top:25px;"
      ),
      shiny::uiOutput(ns("editintake"))
    ),
    shinydashboardPlus::box(
      title = "Students",
      status = "maroon",
      solidHeader = TRUE,
      width = 9,
      collapsible = TRUE,
      collapsed = FALSE,
      closable = FALSE,
      icon = shiny::icon("users"),
      
      shiny::fluidRow(
        shiny::column(
          6,
          shinyWidgets::textInputIcon(
            ns("newencryptkey"), label = NULL,
            placeholder = "Encryption",
            inputType = "password",
            icon = shiny::icon("key"),
            width = "100%"
          )
        ),
        shiny::column(
          6,
          shiny::actionButton(
            ns("savestudents"), "Save students", icon = shiny::icon("floppy-disk"),
            style = "background-color:#006600;color:#FFF;
          width:100%;margin-bottom:25px;"
          )
        )
      ),
      rhandsontable::rHandsontableOutput(ns("students"))
    )
  )
}

