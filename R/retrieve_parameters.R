#' @name retrieve_parameters
#' @title Retrieve parameters
#' @author Nicolas Mangin
#' @description Get the question parameters when it is produced for a test.
#' @param ex_name Character. Name of the .Rmd file being processed.
#' @param pkgname Character. Name of the package containing the .Rmd file being processed.
#' @return Parameters for the questions.
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr case_when
#' @importFrom stats runif
#' @export


retrieve_parameters <- function(ex_name, pkgname) {
  test_parameters <- NULL

  wdir <- gsub("tmp", "rmd", getwd())
  testsolpath <- paste0(wdir, "/specifications.RData")
  if (file.exists(testsolpath)) {
    load(file = testsolpath)
  } else {
    test_or_solution <- "solution"
    type_table <- "html"
  }

  parampath <- paste0(wdir, "/test_parameters.RData")
  if (file.exists(parampath)) {
    load(file = parampath)
    exercise <- test_parameters %>%
      dplyr::filter(exname == ex_name) %>%
      as.data.frame()

    extype <- exercise$extype[1]
    exname <- exercise$exname[1]
    questionid <- exercise$question_id[1]
    if (exercise$showexname[1]) {
      showexname <- paste0(exname, " - ")
    } else showexname <- ""

    if (exercise$show_difficulty[1]) {
      if (exercise$show_points[1]) {
        showdiffpoints <- paste0(
          "(",
          exercise$difficulty[1],
          ", ",
          exercise$points[1],
          ifelse(exercise$points[1] == 1, " point)", " points)")
        )
      } else {
        showdiffpoints <- paste0(
          "(",
          exercise$difficulty[1],
          ")"
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
    currency <- exercise$currency[1]
  } else {
    extype <- "schoice"
    exname <- ex_name
    questionid <- ex_name
    showexname <- paste0(exname, " - ")
    showdiffpoints <- ""
    seed <- floor(1000 + stats::runif(1) * 8999)
    alternatives <- 5
    currency <- "euro"
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
