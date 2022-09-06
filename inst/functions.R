

list_importsFrom <- function(file, exclude = c()){
  lines <- base::readLines(file)
  calls <- stringr::str_extract_all(lines,"[a-z,A-Z,0-9,-,_,\\.]+::[a-z,A-Z,0-9,-,_,\\.]+\\(", simplify = TRUE)
  calls <- stringr::str_remove_all(base::trimws(base::as.vector(base::unlist(calls))), "\\(")
  calls <- base::unique(calls[base::nchar(calls) > 0])
  funs <- tibble::tibble(calls = calls) |>
    tidyr::separate(calls, into = c("packages","functions"), sep = "::") |>
    dplyr::arrange(packages, functions) |>
    dplyr::filter(!(packages %in% exclude)) |>
    dplyr::mutate(import = base::paste0("#' @importFrom ", packages, " ", functions))
  base::writeLines(funs$import)
}



#course_paths <- teachR::course_set_paths("/Volumes/Macintosh HD/Users/nicolas/Dropbox/5-Education/Courses/management_accounting")
#course_data <- base::list()