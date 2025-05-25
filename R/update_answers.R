#' @name update_answers
#' @title Update answers to tests
#' @author Nicolas Mangin
#' @description Function compiling all students' answers into a single database.
#' @param course_paths List. Paths to the different folders and databases on local disk.
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @importFrom stringr str_remove_all
#' @importFrom tidyr separate
#' @importFrom tidyr unnest
#' @export



update_answers <- function(course_paths){
  
  paths <- NULL
  intake <- NULL
  studentid <- NULL
  language <- NULL
  
  answers <- tibble::tibble(
    intake = base::list.files(course_paths$subfolders$answers, pattern = "\\.csv", recursive = FALSE, full.names = FALSE),
    paths = base::list.files(course_paths$subfolders$answers, pattern = "\\.csv", recursive = FALSE, full.names = TRUE)
  ) |>
    tidyr::separate(intake, into = c("test","intake","language"), sep = "-") |>
    dplyr::mutate(
      language = purrr::map_chr(language, stringr::str_remove_all, pattern = "\\.csv"),
      answers = purrr::map(paths, readr::read_csv, col_types = "cTTcccn")
    ) |>
    dplyr::select(-paths) |>
    tidyr::unnest(answers) |>
    dplyr::filter(studentid != "x")
  
  base::save(answers, file = course_paths$databases$answers)
}
