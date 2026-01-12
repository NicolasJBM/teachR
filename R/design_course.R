#' @name design_course
#' @title Launch teaching app
#' @author Nicolas Mangin
#' @description Launcher 
#' @importFrom shiny runApp
#' @export

design_course <- function() {
  appFile <- base::paste0(base::path.package("teachR"), "/app/design_course.R")
  shiny::runApp(appFile, display.mode = "normal")
}
