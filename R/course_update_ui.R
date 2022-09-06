#' @name course_update_ui
#' @title Update documents and data
#' @author Nicolas Mangin
#' @description Module facilitating the update of underlying course data.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return A list of folder paths based on a standard course structure and used by the application.
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny h6
#' @importFrom shiny NS
#' @importFrom shiny span
#' @export


course_update_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        2, shiny::h5("Update:")
      ),
      shiny::column(
        2,
        shiny::actionButton(
          ns("updatedocuments"),
          shiny::span("Docs", title = "Scan documents, read tags, and update the document database accordingly."),
          style = "background-color:#000066;color:#FFF; width:100%;"
        )
      ),
      shiny::column(
        2,
        shiny::actionButton(
          ns("updatetags"),
          shiny::span("Tags", title = "List tags in the document database and update the tag database accordingly. Tags are only added. To remove or edit tags, go in curate."),
          style = "background-color:#330033;color:#FFF; width:100%;"
        )
      ),
      shiny::column(
        2,
        shiny::actionButton(
          ns("updatetrees"),
          shiny::span("Trees", title = "Synchronise trees with the documents database: add missing entries as unclassified, remove extra entries, update titles."),
          style = "background-color:#330033;color:#FFF; width:100%;"
        )
      ),
      shiny::column(
        2,
        shiny::actionButton(
          ns("updatedata"), shiny::span("Data", title = "Gather ratings, comments, views, tests, and results in clean databases for subsequent analyses."),
          style = "background-color:#660000;color:#FFF; width:100%;"
        )
      ),
      shiny::column(
        2,
        shiny::actionButton(
          ns("updatestatistics"), shiny::span("Stats", title = "Count observations, compute basic statistics as well as Item Response Theory statistics for each document and item."),
          style = "background-color:#660000;color:#FFF; width:100%;"
        )
      )
    )
  )
}

