
path <- "D:/Dropbox/5-Education/Courses/management_accounting/basis/1_documents/1_original"
pattern <- "teachR::dbl"
replacement <- "dbl"
files <- base::list.files(path, full.names = TRUE)

for (file in files){
  lines <- readLines(file)
  if (any(stringr::str_detect(lines, pattern))) {
    lines <- stringr::str_replace_all(lines, pattern, replacement)
    writeLines(lines, file)
  }
}


path <- "R"
string <- "shinyAce"
funs <- list.files(path, full.names = TRUE)

for (file in funs){
  lines <- readLines(file)
  if (any(stringr::str_detect(lines, string))) rstudioapi::navigateToFile(file)
}





