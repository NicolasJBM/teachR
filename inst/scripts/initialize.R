parameters <- teachR::retrieve_parameters(
  wdir = getwd(),
  questionid = questionid
)
for (i in 1:length(parameters)) assign(names(parameters)[[i]], parameters[[i]])
set.seed(seed)