#' @name update_students
#' @title Update student list
#' @author Nicolas Mangin
#' @description Function listing all students and gathering all information about them.
#' @param course_paths List. Paths to the different folders and databases on local disk.
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export



update_students <- function(course_paths){
  
  paths <- NULL
  studentlist <- NULL
  intake <- NULL
  studentid <- NULL
  
  students <- tibble::tibble(
    intake = base::list.files(course_paths$subfolders$students, pattern = "\\.csv", recursive = FALSE, full.names = FALSE),
    paths = base::list.files(course_paths$subfolders$students, pattern = "\\.csv", recursive = FALSE, full.names = TRUE)
  ) |>
    dplyr::mutate(
      intake = purrr::map_chr(intake, stringr::str_remove_all, pattern = "\\.csv"),
      studentlist = purrr::map(paths, readr::read_csv, col_types = "ccccccccc")
    ) |>
    dplyr::select(-paths) |>
    tidyr::unnest(studentlist)
  
  base::save(students, file = base::paste0(course_paths$subfolders$students, "/students.RData"))
}
