#' @name update_comments
#' @title Update comments
#' @author Nicolas Mangin
#' @description Function listing comments and gathering all information about them.
#' @param course_paths List. Paths to the different folders and databases on local disk.
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export



update_comments <- function(course_paths){
  
  paths <- NULL
  allcomments <- NULL
  intake <- NULL
  timestamp <- NULL
  
  comments <- tibble::tibble(
    intake = base::list.files(course_paths$subfolders$comments, pattern = "\\.csv", recursive = FALSE, full.names = FALSE),
    paths = base::list.files(course_paths$subfolders$comments, pattern = "\\.csv", recursive = FALSE, full.names = TRUE)
  ) |>
    dplyr::mutate(
      intake = purrr::map_chr(intake, stringr::str_remove_all, pattern = "\\.csv"),
      allcomments = purrr::map(paths, readr::read_csv)
    ) |>
    dplyr::select(-paths) |>
    tidyr::unnest(allcomments) |>
    dplyr::filter(timestamp != "x")
  
  base::save(comments, file = base::paste0(course_paths$subfolders$comments, "/comments.RData"))
}
