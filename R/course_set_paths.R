#' @name course_set_paths
#' @title Complete course folder paths
#' @author Nicolas Mangin
#' @description Retrieve a list of paths to relevant databases or subfolders in the selected course folder on the local computer.
#' @param course_folder Character. Course folder as selected by the user.
#' @return A list of folder paths based on a specified course structure and used by the application.
#' @importFrom dplyr mutate
#' @importFrom readr read_csv
#' @importFrom shiny addResourcePath
#' @importFrom stringr str_remove
#' @export


course_set_paths <- function(course_folder){
  
  path <- NULL
  
  course_folder <- stringr::str_remove(course_folder, "/Volumes/Macintosh HD")
  
  if (!base::is.null(course_folder) & !base::is.na(course_folder) & base::nchar(course_folder)){
    
    subfolders <- readr::read_csv(base::paste0(course_folder, "/subfolders.csv"), col_types = "cc") |>
      dplyr::mutate(path = base::paste0(course_folder, path))
    subfolders <- base::split(subfolders$path, subfolders$name)
    
    databases <- readr::read_csv(base::paste0(course_folder, "/databases.csv"), col_types = "cc") |>
      dplyr::mutate(path = base::paste0(course_folder, path))
    databases <- base::split(databases$path, databases$name)
    
    shiny::addResourcePath("edition", base::as.character(subfolders$edit))
    
    course_paths <- base::list(
      subfolders = subfolders,
      databases = databases
    )
    
  } else {
    
    course_paths <- NA
    
  }
  
  return(course_paths)
}
