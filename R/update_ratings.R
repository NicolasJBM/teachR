#' @name update_ratings
#' @title Update ratings
#' @author Nicolas Mangin
#' @description Function listing ratings and gathering all information about them.
#' @param course_paths List. Paths to the different folders and databases on local disk.
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @importFrom tidyr separate
#' @export



update_ratings <- function(course_paths){
  
  paths <- NULL
  allratings <- NULL
  intake <- NULL
  timestamp <- NULL
  section <- NULL
  
  ratings <- tibble::tibble(
    intake = base::list.files(course_paths$subfolders$ratings, pattern = "\\.csv", recursive = FALSE, full.names = FALSE),
    paths = base::list.files(course_paths$subfolders$ratings, pattern = "\\.csv", recursive = FALSE, full.names = TRUE)
  ) |>
    dplyr::mutate(
      intake = purrr::map_chr(intake, stringr::str_remove_all, pattern = "\\.csv"),
      allratings = purrr::map(paths, readr::read_csv, col_types = "Tcd")
    ) |>
    dplyr::select(-paths) |>
    tidyr::unnest(allratings) |>
    dplyr::filter(timestamp != "2025-01-01 00:00:00") |>
    tidyr::separate(section, into = c("group","file"), sep = "-")
  
  base::save(ratings, file = base::paste0(course_paths$subfolders$ratings, "/ratings.RData"))
}
