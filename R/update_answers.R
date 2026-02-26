#' @name update_answers
#' @title Update answers to tests
#' @author Nicolas Mangin
#' @description Function compiling all students' answers into a single database.
#' @param course_paths List. Paths to the different folders and databases on local disk.
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom tidyr unite
#' @importFrom tidyr unnest
#' @export



update_answers <- function(course_paths){
  
  files <- NULL
  path <- NULL
  
  testpath <- course_paths$subfolders$tests
  
  tests <- base::list.dirs(testpath, recursive = FALSE, full.names = FALSE)
  tests <- tests[tests != "archives"]
  
  answerspaths <- base::paste0(testpath, "/", tests, "/6_answers")
  answernbr <- base::length(answerspaths)
  
  allanswers <- base::list(answernbr)
  for (i in base::seq_len(answernbr)){
    allanswers[[i]] <- base::list.files(answerspaths[i], full.names = FALSE, recursive = TRUE, include.dirs = FALSE)
  }
  answers <- tibble::tibble(files = allanswers) |>
    dplyr::bind_rows() |>
    dplyr::mutate(path = answerspaths) |>
    tidyr::unnest(files) |>
    tidyr::unite("path", path, files, sep = "/") |>
    dplyr::mutate(answers = purrr::map(path, readr::read_csv, col_types = "cccccccd")) |>
    dplyr::select(-path) |>
    tidyr::unnest(answers)
  
  if (base::file.exists(course_paths$databases$answers)){
    base::file.remove(course_paths$databases$answers)
  }
  
  base::save(answers, file = course_paths$databases$answers)
}
