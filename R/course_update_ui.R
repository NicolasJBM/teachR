#' @name course_update_ui
#' @title Update documents and data
#' @author Nicolas Mangin
#' @description Module facilitating the update of underlying course documents, trees, data, and statistics.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny span
#' @export


course_update_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::actionButton(
          ns("updatedocuments"),
          shiny::span("Update documents", title = "Scan documents, read tags, and update the document database accordingly."),
          icon = shiny::icon("file-pen"),
          style = "background-color:#006699;color:#FFF; width:100%;"
        )
      ),
      shiny::column(
        4,
        shiny::actionButton(
          ns("updatetrees"),
          shiny::span("Update trees", title = "Synchronise trees with the documents database: add missing entries as unclassified, remove extra entries, update titles."),
          icon = shiny::icon("sitemap"),
          style = "background-color:#006699;color:#FFF; width:100%;"
        )
      ),
      shiny::column(
        4,
        shiny::actionButton(
          ns("updatestatistics"),
          shiny::span("Update statistics", title = "Count observations, compute basic statistics as well as Item Response Theory statistics for each document and item."),
          icon = shiny::icon("percent"),
          style = "background-color:#006699;color:#FFF; width:100%;"
        )
      )
    )
  )
}

