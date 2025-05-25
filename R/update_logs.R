#' @name update_logs
#' @title Update logs
#' @author Nicolas Mangin
#' @description Function listing logs and gathering all information about them.
#' @param course_paths List. Paths to the different folders and databases on local disk.
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export



update_logs <- function(course_paths){
  
  paths <- NULL
  alllogs <- NULL
  intake <- NULL
  
  logs <- tibble::tibble(
    intake = base::list.files(course_paths$subfolders$logs, pattern = "\\.csv", recursive = FALSE, full.names = FALSE),
    paths = base::list.files(course_paths$subfolders$logs, pattern = "\\.csv", recursive = FALSE, full.names = TRUE)
  ) |>
    dplyr::mutate(
      intake = purrr::map_chr(intake, stringr::str_remove_all, pattern = "\\.csv"),
      alllogs = purrr::map(paths, readr::read_csv, col_types = "iTcccc")
    ) |>
    dplyr::select(-paths) |>
    tidyr::unnest(alllogs) |>
    dplyr::filter(log != 0)
  
  base::save(logs, file = base::paste0(course_paths$subfolders$logs, "/logs.RData"))
}
