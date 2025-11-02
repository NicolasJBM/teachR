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
  old <- NULL
  path <- NULL
  solution <- NULL
  solutionpath <- NULL
  
  if (base::file.exists(course_paths$databases$solutions)){
    base::load(course_paths$databases$solutions)
  } else {
    solutions <- tibble::tibble(version = NA)
  }
  
  testpath <- course_paths$subfolders$tests
  
  tests <- base::list.dirs(testpath, recursive = FALSE, full.names = FALSE)
  solutionspaths <- base::paste0(testpath, "/", tests, "/4_solutions")
  testnbr <- base::length(solutionspaths)
  
  allsolutions <- base::list(testnbr)
  for (i in base::seq_len(testnbr)){
    allsolutions[[i]] <- base::list.files(solutionspaths[i], full.names = FALSE)
  }
  allsolutions <- tibble::tibble(
    solutionpath = solutionspaths,
    solution = allsolutions
  ) |>
    tidyr::unnest(solution) |>
    dplyr::mutate(old = purrr::map_lgl(
      solution,
      function(x,y) stringr::str_remove(x, ".csv") %in% y$version,
      solutions
    ))
  
  newsolutions <- allsolutions |>
    dplyr::filter(old == FALSE) |>
    dplyr::select(-old) |>
    tidyr::unite("path", solutionpath, solution, sep = "/") |>
    dplyr::mutate(solutions = purrr::map(path, readr::read_csv, col_types = "ccdcccccdccdcccdd")) |>
    dplyr::select(-path) |>
    tidyr::unnest(solutions)
  
  if (base::file.exists(course_paths$databases$solutions)){
    solutions <- dplyr::bind_rows(solutions, newsolutions)
  } else {
    solutions <- newsolutions
  }
  
  base::save(solutions, file = course_paths$databases$solutions)
}



