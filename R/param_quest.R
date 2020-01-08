#' Function to retrieve or generate parameters used when questions are generated.
#' @param wdir        Character. Working directory.
#' @param question_id Character. ID of the question.
#' @param alttype     Character. force the type of question: "mcq" or "open"
#' @param altlevel    Character. force the type of level for the question: "2 Understand" or "3 Apply"
#' @return Parameters for the questions.
#' @importFrom dplyr filter
#' @export


param_quest <- function(wdir = "",
                        question_id = "",
                        alttype = NULL,
                        altlevel = NULL) {

  # Bind variables
  choices <- NULL
  QN <- NULL
  show_question_id <- NULL
  show_question_pt <- NULL

  if (file.exists(gsub("/tmp/", "/", paste0(wdir, "/parameters/exam_parameters.RData")))) {
    load(file = gsub("/tmp/", "/", paste0(getwd(), "/parameters/exam_parameters.RData")))
    question_info <- dplyr::filter(as.data.frame(choices), QN == question_id)
    quest_level <- question_info$BL[[1]]
    exasolu <- question_info$ES[[1]]
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
    exasolu <- "solution"
    if (is.null(alttype)) type_quest <- "mcq" else type_quest <- alttype
    if (is.null(altlevel)) quest_level <- "3 Apply" else quest_level <- altlevel
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
  
  if (type_quest == "mcq") {
    reqexpl <- ""
    extype <- "schoice"
  } else if(quest_level %in% c("3 Apply","4 Analyze","5 Evaluate")) {
    reqexpl <- "Please provide detailed computations and adequate explanations whenever applicable."
    extype <- "string"
  } else {
    reqexpl <- "Please provide relevant justifications, explanations, and illustrations whenever applicable."
    extype <- "string"
  }
  
  
  
  parameters <- list(
    reqexpl = reqexpl,
    extype = extype,
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
