#' @name edit_path_file
#' @title Open the file specifying the path to courses
#' @author Nicolas Mangin
#' @description Opener 
#' @importFrom rstudioapi navigateToFile
#' @export

edit_path_file <- function() {
  pathfile <- base::paste0(base::path.package("teachR"), "/app/mainpath.txt")
  rstudioapi::navigateToFile(pathfile)
}
