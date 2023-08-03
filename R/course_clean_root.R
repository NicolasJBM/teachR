#' @name course_clean_root
#' @title Remove temporary files in the working directory
#' @author Nicolas Mangin
#' @description Remove non-essential files and folders from the root of the project. These files are typically created when .Rmd documents are knitted.
#' @importFrom stringr str_detect
#' @export


course_clean_root <- function(){
  files <- base::list.files(recursive = FALSE)
  pngfiles <- files[stringr::str_detect(files, ".png$")]
  base::suppressMessages(base::file.remove(pngfiles))
  if (base::dir.exists("figure")) base::unlink("figure", recursive = TRUE)
}
