
path <- "D:/Dropbox/5-Education/Courses/management_accounting/basis/5_functions"
files <- base::list.files(path, full.names = TRUE)

pattern <- "teachR::dbl"
replacement <- "dbl"

for (file in files){
  lines <- readLines(file)
  if (any(stringr::str_detect(lines, pattern))) {
    lines <- stringr::str_replace_all(lines, pattern, replacement)
    writeLines(lines, file)
  }
}


path <- "R"
string <- "rename"
funs <- list.files(path, full.names = TRUE)

for (file in funs){
  lines <- readLines(file)
  if (any(stringr::str_detect(lines, string))) rstudioapi::navigateToFile(file)
}





