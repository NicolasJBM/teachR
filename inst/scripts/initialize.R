questionid <- stringr::str_remove(knitr::current_input(), ".Rmd")
parameters <- teachR::retrieve_parameters(
  wdir = getwd(),
  question_id = questionid
)
for (i in 1:length(parameters)) assign(names(parameters)[[i]], parameters[[i]])
set.seed(seed)