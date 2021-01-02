#' @name retrieve_parameters
#' @title Retrieve parameters
#' @author Nicolas Mangin
#' @description Get the question parameters when it is prodiced for a test.
#' @param wdir        Character. Path to working directory.
#' @param questionid Character. ID of the question.
#' @return Parameters for the questions.
#' @importFrom dplyr filter
#' @importFrom stats runif
#' @export


retrieve_parameters <- function(wdir = "",
                                questionid = "") {
  if (file.exists(
    gsub("/tmp/", "/", paste0(getwd(), "/parameters/exam_parameters.RData"))
  )) {
    load(file = gsub(
      "/tmp/",
      "/",
      paste0(getwd(), "/parameters/exam_parameters.RData")
    ))

    exercise <- exam_parameters %>%
      dplyr::filter(question_id == questionid) %>%
      as.data.frame()

    extype <- exercise$extype[1]
    exname <- exercise$exname[1]

    if (exercise$show_difficulty[1]) {
      if (exercise$show_points[1]) {
        points <- paste0(
          "(",
          exercise$difficulty[1],
          " difficulty, ",
          exercise$points[1],
          ifelse(exercise$points[1] == 1, " point)", " points)")
        )
      } else {
        points <- paste0(
          "(",
          exercise$difficulty[1],
          " difficulty)"
        )
      }
    } else {
      if (exercise$show_points[1]) {
        points <- paste0(
          "(",
          exercise$points[1],
          ifelse(exercise$points[1] == 1, " point)", " points)")
        )
      } else {
        points <- ""
      }
    }

    seed <- exercise$seed[1]
    alternatives <- exercise$alternatives[1]
    type_table <- exercise$type_table[1]
    currency <- exercise$currency[1]
    test_or_solution <- exercise$test_or_solution[1]
  } else {
    extype <- "schoice"
    exname <- questionid
    points <- ""
    seed <- ceiling(stats::runif(1)*10000)
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
    points = points,
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
