#' @name retrieve_parameters
#' @title Retrieve parameters
#' @author Nicolas Mangin
#' @description Get the question parameters when it is produced for a test.
#' @param ex_name Character. Name of the .Rmd file being processed.
#' @param pkgname Character. Name of the package containing the .Rmd file being processed.
#' @return Parameters for the questions.
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom stats runif
#' @export


retrieve_parameters <- function(ex_name, pkgname) {
  test_parameters <- NULL
  
  wdir <- gsub("tmp","rmd", getwd())
  parampath <- paste0(wdir, "/test_parameters.RData")
  if (file.exists(parampath)) {
    
    load(file = parampath)
    exercise <- test_parameters %>%
      dplyr::filter(exname == ex_name) %>%
      as.data.frame()

    extype <- exercise$extype[1]
    exname <- exercise$exname[1]
    questionid <- exercise$question_id[1]
    showexname <- paste0(exname, " - ")

    if (exercise$show_difficulty[1]) {
      if (exercise$show_points[1]) {
        showdiffpoints <- paste0(
          "(",
          exercise$difficulty[1],
          " difficulty, ",
          exercise$points[1],
          ifelse(exercise$points[1] == 1, " point)", " points)")
        )
      } else {
        showdiffpoints <- paste0(
          "(",
          exercise$difficulty[1],
          " difficulty)"
        )
      }
    } else {
      if (exercise$show_points[1]) {
        showdiffpoints <- paste0(
          "(",
          exercise$points[1],
          ifelse(exercise$points[1] == 1, " point)", " points)")
        )
      } else {
        showdiffpoints <- ""
      }
    }

    seed <- exercise$seed[1]
    alternatives <- exercise$alternatives[1]
    type_table <- exercise$type_table[1]
    currency <- exercise$currency[1]
    test_or_solution <- exercise$test_or_solution[1]
  } else {
    extype <- "schoice"
    exname <- ex_name
    questionid <- ex_name
    showexname <- paste0(exname, " - ")
    showdiffpoints <- ""
    seed <- 1234 # switch back to random when test finished.
    alternatives <- 5
    type_table <- "html"
    currency <- "euro"
    test_or_solution <- "solution"
  }


  if (type_table == "latex") {
    currencysymb <- switch(currency,
      euro = "\\texteuro",
      dollar = "\\textdollar",
      pound = "\\pounds",
      yen = "\\textyen"
    )
  } else {
    currencysymb <- switch(currency,
      euro = "&euro;",
      dollar = "$",
      pound = "&pound;",
      yen = "&yen;"
    )
  }

  if (type_table == "latex") pctsymb <- "\\%" else pctsymb <- "%"

  parameters <- list(
    extype = extype,
    exname = exname,
    questionid = questionid,
    showexname = showexname,
    showdiffpoints = showdiffpoints,
    seed = seed,
    alternatives = alternatives,
    type_table = type_table,
    currency = currency,
    test_or_solution = test_or_solution,
    currencysymb = currencysymb,
    pctsymb = pctsymb
  )

  return(parameters)
}
