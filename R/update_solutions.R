#' @name update_solutions
#' @title Update test list
#' @author Nicolas Mangin
#' @description Function listing all tests and gathering all information about their content.
#' @param course_paths List. Paths to the different folders and databases on local disk.
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom readr read_csv
#' @importFrom tidyr unnest
#' @export



update_solutions <- function(course_paths){
  
  parameters <- NULL
  paths <- NULL
  files <- NULL
  test <- NULL
  
  tests <- base::list.dirs(course_paths$subfolders$tests, recursive = FALSE, full.names = FALSE)
  
  if (base::file.exists(course_paths$databases$solutions)){
    base::load(course_paths$databases$solutions)
    solutions <- dplyr::filter(solutions, test %in% tests)
    newtests <- base::setdiff(tests, c(base::unique(solutions$test), "archives", "default"))
  } else {
    newtests <- base::setdiff(tests, c("archives", "default"))
  }
  
  newsolutions <- tibble::tibble(
    paths = base::paste0(course_paths$subfolders$tests, "/", newtests)
  ) |>
    dplyr::mutate(paths = base::paste0(paths, "/4_solutions")) |>
    dplyr::mutate(solutions = purrr::map(paths, function(x){
      tibble::tibble(files = base::list.files(x, full.names = TRUE)) |>
        dplyr::mutate(solutions = purrr::map(files, readr::read_csv, col_types = "ccdcccccdccdcccdd")) |>
        dplyr::select(-files) |>
        tidyr::unnest(solutions)
    })) |>
    dplyr::select(-paths) |>
    tidyr::unnest(solutions)
  
  if (base::file.exists(course_paths$databases$solutions)){
    solutions <- dplyr::bind_rows(solutions, newsolutions)
  } else {
    solutions <- newsolutions
  }
  
  solutions$solutions <- NULL
  
  base::save(solutions, file = course_paths$databases$solutions)
}



