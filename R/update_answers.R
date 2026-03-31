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
  
  testpath <-  "C:/Users/nicol/Dropbox/5-Education/scholR/financial_analysis_valuation/4_delivering/6_tests" #course_paths$subfolders$tests
  
  tests <- base::list.dirs(testpath, recursive = FALSE, full.names = FALSE)
  tests <- tests[tests != "archives"]
  
  answerspaths <- base::paste0(testpath, "/", tests, "/6_answers")
  answernbr <- base::length(answerspaths)
  
  allanswers <- base::list()
  for (i in base::seq_len(answernbr)){
    allanswers[[i]] <- base::list.files(answerspaths[i], full.names = FALSE, recursive = TRUE, include.dirs = FALSE, pattern = "csv$")
  }
  answers <- tibble::tibble(files = allanswers) |>
    dplyr::bind_rows() |>
    dplyr::mutate(path = answerspaths) |>
    tidyr::unnest(files) |>
    tidyr::unite("path", path, files, sep = "/", remove = FALSE) |>
    dplyr::mutate(answers = purrr::map(path, readr::read_csv, col_types = "cccccd")) |>
    tidyr::unnest(answers) |>
    tidyr::separate(files, into = c("test","intake","extension"), sep = "-") |>
    dplyr::select(-path, -extension)
  
  if (base::file.exists(course_paths$databases$answers)){
    base::file.remove(course_paths$databases$answers)
  }
  
  base::save(answers, file = course_paths$databases$answers)
}
