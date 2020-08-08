#' Function to retrieve or generate parameters used when questions are generated.
#' @param wdir        Character. Working directory.
#' @param question_id Character. ID of the question.
#' @return Parameters for the questions.
#' @importFrom dplyr filter
#' @export


param_quest <- function(wdir = "",
                        question_id = "") {

  # Bind variables
  choices <- NULL
  QN <- NULL
  show_question_id <- NULL
  show_question_pt <- NULL

  if (
    file.exists(gsub("/tmp/", "/", paste0(wdir, "/parameters/exam_parameters.RData"))) &
    file.exists(gsub("/tmp/", "/", paste0(wdir, "/parameters/exasolu.RData")))
    ) {
    load(file = gsub("/tmp/", "/", paste0(getwd(), "/parameters/exam_parameters.RData")))
    load(file = gsub("/tmp/", "/", paste0(getwd(), "/parameters/exasolu.RData")))
    question_info <- dplyr::filter(as.data.frame(choices), QN == question_id)
    quest_level <- question_info$BL[[1]]
    if (show_question_id == TRUE) {
      txt_question_id <- paste0(question_id, ". ")
    } else {
      txt_question_id <- ""
    }
    if (show_question_pt == TRUE) {
      points <- as.integer(question_info$PT[[1]])
      if (points == 1) unit <- "point" else unit <- "points"
      points <- paste0("(", points, " ", unit, ")")
    } else {
      points <- ""
    }
    seed <- as.integer(question_info$SD[[1]])
  } else {
    type_quest <- "mcq"
    exasolu <- "solution"
    txt_question_id <- paste0(question_id, ". ")
    points <- ""
    type_table <- "html"
    currency <- "euro"
    seed <- sample(c(1:9999), 1)
    alternatives <- 5
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
    type_quest = type_quest,
    txt_question_id = txt_question_id,
    points = points,
    type_table = type_table,
    currency = currency,
    currencysymb = currencysymb,
    pctsymb = pctsymb,
    seed = seed,
    alternatives = alternatives,
    exasolu = exasolu
  )

  return(parameters)
}
