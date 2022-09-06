#' @name course_references_ui
#' @title Load course references
#' @author Nicolas Mangin
#' @description Module facilitating the loading of all relevant references for the course.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Tibble with references.
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fileInput
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny NS
#' @export


course_references_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        8,
        shiny::fileInput(
          ns("referencefile"),
          "Select references:",
          width = "100%",
          accept = ".RData"
        )
      ),
      shiny::column(
        4,
        shiny::actionButton(
          ns("updateref"),
          "Update references",
          icon = shiny::icon("rotate"),
          style = "background-color:#003366;color:#FFF;width:100%;margin-top:25px;"
        )
      )
    )
  )
}

