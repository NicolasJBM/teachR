#' @name update_tests
#' @title Update test list
#' @author Nicolas Mangin
#' @description Function listing all tests and gathering all information about their content.
#' @param course_paths List. Paths to the different folders and databases on local disk.
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export



update_tests <- function(course_paths){
  
  parameters <- NULL
  paths <- NULL
  test_parameters <- NULL
  
  tests <- tibble::tibble(paths = base::list.dirs(course_paths$subfolders$tests, recursive = FALSE)) |>
    dplyr::filter(!stringr::str_detect(paths, "archives"),!stringr::str_detect(paths, "default")) |>
    dplyr::mutate(paths = base::paste0(paths, "/test_parameters.RData")) |>
    dplyr::mutate(parameters = purrr::map(paths, function(x){
      base::load(x)
      test_parameters
    })) |>
    dplyr::select(-paths) |>
    tidyr::unnest(parameters)
  
  base::save(tests, file = course_paths$databases$tests)
}
