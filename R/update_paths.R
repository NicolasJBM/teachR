#' @name update_paths
#' @title Update student list
#' @author Nicolas Mangin
#' @description Function listing all students and gathering all information about them.
#' @param course_paths List. Paths to the different folders and databases on local disk.
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export



update_paths <- function(course_paths){
  
  paths <- NULL
  path <- NULL
  
  pathfiles <- tibble::tibble(
    path = base::list.files(course_paths$subfolders$paths, pattern = "\\.xlsx", recursive = FALSE, full.names = FALSE),
    paths = base::list.files(course_paths$subfolders$paths, pattern = "\\.xlsx", recursive = FALSE, full.names = TRUE)
  ) |>
    dplyr::mutate(path = purrr::map_chr(path, stringr::str_remove_all, pattern = "\\.xlsx"))
  
  outcomes <- pathfiles |>
    dplyr::mutate(outcomes = purrr::map(paths, readxl::read_excel, sheet = "outcomes")) |>
    dplyr::select(-paths) |>
    tidyr::unnest(outcomes)
  
  connections <- pathfiles |>
    dplyr::mutate(connections = purrr::map(paths, readxl::read_excel, sheet = "connections")) |>
    dplyr::select(-paths) |>
    tidyr::unnest(connections)
  
  outlabels <- pathfiles |>
    dplyr::mutate(outlabels = purrr::map(paths, readxl::read_excel, sheet = "outlabels")) |>
    dplyr::select(-paths) |>
    tidyr::unnest(outlabels)
  
  activities <- pathfiles |>
    dplyr::mutate(activities = purrr::map(paths, readxl::read_excel, sheet = "activities")) |>
    dplyr::select(-paths) |>
    tidyr::unnest(activities)
  
  actlabels <- pathfiles |>
    dplyr::mutate(actlabels = purrr::map(paths, readxl::read_excel, sheet = "actlabels")) |>
    dplyr::select(-paths) |>
    tidyr::unnest(actlabels)
  
  attributes <- pathfiles |>
    dplyr::mutate(attributes = purrr::map(paths, readxl::read_excel, sheet = "attributes")) |>
    dplyr::select(-paths) |>
    tidyr::unnest(attributes)
  
  paths <- base::list(
    outcomes = outcomes,
    connections = connections,
    outlabels = outlabels,
    activities = activities,
    actlabels = actlabels,
    attributes = attributes
  )
  
  base::save(paths, file = base::paste0(course_paths$subfolders$paths, "/paths.RData"))
}
