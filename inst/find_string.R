
path <- "/Volumes/Macintosh HD/Users/nicolas/Dropbox/5-Education/Courses/management_accounting/basis/1_documents/1_original"

path <- "R"
funs <- list.files(path, full.names = TRUE)

for (file in funs){
  lines <- readLines(file)
  if (any(stringr::str_detect(lines, 'tree_'))) {
    lines <- stringr::str_replace_all(lines, 'tree_', 'editR::add_')
    writeLines(lines, file)
  }
}


for (file in funs){
  lines <- readLines(file)
  if (any(stringr::str_detect(lines, 'trees_structure_textbook'))) rstudioapi::navigateToFile(file)
}

