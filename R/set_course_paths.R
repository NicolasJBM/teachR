#' @name set_course_paths
#' @title Complete course folder paths
#' @author Nicolas Mangin
#' @description Retrieve a list of paths to relevant databases or subfolders in the selected course folder on the local computer.
#' @param project_folder Character. Course folder as selected by the user.
#' @return A list of folder paths based on a specified course structure and used by the application.
#' @importFrom dplyr mutate
#' @importFrom readr read_csv
#' @importFrom shiny addResourcePath
#' @importFrom stringr str_remove
#' @export


set_course_paths <- function(project_folder){
  
  path <- NULL
  
  project_folder <- stringr::str_remove(project_folder, "/Volumes/Macintosh HD")
  
  if (!base::is.null(project_folder) & !base::is.na(project_folder) & base::nchar(project_folder)){
    
    subfolders <- readr::read_csv(base::paste0(project_folder, "/subfolders.csv"), col_types = "cc") |>
      dplyr::mutate(path = base::paste0(project_folder, path))
    subfolders <- base::split(subfolders$path, subfolders$name)
    
    databases <- readr::read_csv(base::paste0(project_folder, "/databases.csv"), col_types = "cc") |>
      dplyr::mutate(path = base::paste0(project_folder, path))
    databases <- base::split(databases$path, databases$name)
    
    shiny::addResourcePath("edition", base::as.character(subfolders$edit))
    
    project_paths <- base::list(
      subfolders = subfolders,
      databases = databases
    )
    
  } else {
    
    project_paths <- NA
    
  }
  
  return(project_paths)
}
