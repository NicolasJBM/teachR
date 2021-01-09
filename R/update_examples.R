#' @name update_examples
#' @title Create HTML examples
#' @author Nicolas Mangin
#' @description For each .Rmd file in the package, create a HTML example
#' @importFrom knitr knit2html
#' @importFrom stringr str_replace_all

update_examples <- function() {
  files <- list.files("inst/questions")
  for (file in files) {
    knitr::knit2html(
      file,
      output = stringr::str_replace_all(
        stringr::str_replace_all(file, ".Rmd", ".html"),
        "questions",
        "examples"
      )
    )
  }
}
