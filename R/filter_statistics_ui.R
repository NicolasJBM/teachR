#' @name filter_statistics_ui
#' @title Select documents from statistics (UI)
#' @author Nicolas Mangin
#' @description Module allowing the user to select documents by clicking on the different sections of a classification statistics.
#' @param id ID of the module to connect the user interface to the appropriate server side.
#' @return Tibble. List of selected documents.
#' @import shiny
#' @export

filter_statistics_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        4,
        shinyWidgets::prettySwitch(
          ns("applyratingsfilters"), "Apply ratings filters", inline = TRUE
        ),
        shiny::uiOutput(ns("ratings_filters"))
      ),
      shiny::column(
        4,
        shinyWidgets::prettySwitch(
          ns("applyviewsfilters"), "Apply views filters", inline = TRUE
        ),
        shiny::uiOutput(ns("views_filters")),
      ),
      shiny::column(
        4,
        shinyWidgets::prettySwitch(
          ns("applyresultsfilters"), "Apply results filters", inline = TRUE
        ),
        shiny::uiOutput(ns("results_filters"))
      )
    ),
    shiny::tags$hr(), shiny::textOutput(ns("count_after_stats"))
  )
}

