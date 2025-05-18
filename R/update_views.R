#' @name update_views
#' @title Update views
#' @author Nicolas Mangin
#' @description Function listing views and gathering all information about them.
#' @param course_paths List. Paths to the different folders and databases on local disk.
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export



update_views <- function(course_paths){
  
  paths <- NULL
  allviews <- NULL
  intake <- NULL
  Content <- NULL
  
  views <- tibble::tibble(
    intake = base::list.files(course_paths$subfolders$views, pattern = "\\.csv", recursive = FALSE, full.names = FALSE),
    paths = base::list.files(course_paths$subfolders$views, pattern = "\\.csv", recursive = FALSE, full.names = TRUE)
  ) |>
    dplyr::mutate(
      intake = purrr::map_chr(intake, stringr::str_remove_all, pattern = "\\.csv"),
      allviews = purrr::map(paths, readr::read_csv)
    ) |>
    dplyr::select(-paths) |>
    tidyr::unnest(allviews) |>
    dplyr::filter(Content != "x")
  
  base::save(views, file = base::paste0(course_paths$subfolders$views, "/views.RData"))
}
