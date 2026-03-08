#' @name develop_course
#' @title Launch teaching app
#' @author Nicolas Mangin
#' @description Launcher 
#' @importFrom shiny runApp
#' @export

develop_course <- function() {
  appFile <- base::paste0(base::path.package("teachR"), "/app/develop_course.R")
  shiny::runApp(appFile, display.mode = "normal")
}
