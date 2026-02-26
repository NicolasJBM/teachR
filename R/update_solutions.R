#' @name update_solutions
#' @title Update test list
#' @author Nicolas Mangin
#' @description Function listing all tests and gathering all information about their content.
#' @param course_paths List. Paths to the different folders and databases on local disk.
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom tidyr unite
#' @importFrom tidyr unnest
#' @export



update_solutions <- function(course_paths){
  
  files <- NULL
  path <- NULL
  
  testpath <- course_paths$subfolders$tests
  
  tests <- base::list.dirs(testpath, recursive = FALSE, full.names = FALSE)
  tests <- tests[tests != "archives"]
  
  solutionspaths <- base::paste0(testpath, "/", tests, "/4_solutions")
  solutionnbr <- base::length(solutionspaths)
  
  allsolutions <- base::list(solutionnbr)
  for (i in base::seq_len(solutionnbr)){
    allsolutions[[i]] <- base::list.files(solutionspaths[i], full.names = FALSE, recursive = TRUE, include.dirs = FALSE)
  }
  solutions <- tibble::tibble(files = allsolutions) |>
    dplyr::bind_rows() |>
    dplyr::mutate(path = solutionspaths) |>
    tidyr::unnest(files) |>
    tidyr::unite("path", path, files, sep = "/") |>
    dplyr::mutate(solutions = purrr::map(path, readr::read_csv, col_types = "ccdcccccdccdcccdd")) |>
    dplyr::select(-path) |>
    tidyr::unnest(solutions)
  
  if (base::file.exists(course_paths$databases$solutions)){
    base::file.remove(course_paths$databases$solutions)
  }
  
  base::save(solutions, file = course_paths$databases$solutions)
}



