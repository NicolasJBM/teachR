

funs <- list.files("R", full.names = TRUE)

pkg <- list()

for (file in funs){
  lines <- base::readLines(file)
  calls <- stringr::str_extract_all(lines,"[a-z,A-Z,0-9,-,_,\\.]+::[a-z,A-Z,0-9,-,_,\\.]+\\(", simplify = TRUE)
  calls <- stringr::str_remove_all(base::trimws(base::as.vector(base::unlist(calls))), "\\(")
  calls <- base::unique(calls[base::nchar(calls) > 0])
  funs <- tibble::tibble(calls = calls) |>
    tidyr::separate(calls, into = c("packages","functions"), sep = "::") |>
    dplyr::arrange(packages, functions) |>
    dplyr::filter(!(packages %in% c("base","teachR","bibliogR","chartR","stats","utils","shiny"))) |>
    dplyr::mutate(
      file = file,
      import = base::paste0("#' @importFrom ", packages, " ", functions)
    )
  pkg[[file]] <- funs
}

pkg <- dplyr::bind_rows(pkg)


