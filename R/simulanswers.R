#' Function creating a test of the exam generator based on the solutions recorded in an rds file.
#' @param exam_id character. ID of the exam.
#' @param correction numeric. Penalty to apply for guesses.
#' @param nbr integer. Number of students to simulate.
#' @param path character. Path to the .rds file direcgtory.
#' @return A list with two elements: answers from students and corresponding grades
#' @importFrom tibble tibble
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize_all
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr case_when
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr starts_with
#' @importFrom tidyr separate
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom webshot webshot
#' @importFrom readxl read_excel
#' @importFrom WriteXLS WriteXLS
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @export

simulanswers <- function(exam_id = NULL,
                             correction = 1 / (5 - 1),
                             nbr = 50,
                             path = getwd()) {

  # Visibly bind variables
  solution <- NULL
  student_id <- NULL
  wrong <- NULL
  question_id <- NULL
  answer <- NULL
  score <- NULL
  question <- NULL
  tosplit <- NULL
  value <- NULL
  type <- NULL

  versionid <- exam_id

  exam <- readRDS(paste0(path, "/", versionid, ".rds"))

  questnbr <- length(exam[[1]])
  questions <- c()
  points <- c()
  solutions <- c()
  for (i in 1:questnbr) questions[i] <- exam$exam1[[i]]$metainfo$name
  for (i in 1:questnbr) points[i] <- exam$exam1[[i]]$metainfo$points
  for (i in 1:questnbr) solutions[i] <- paste0(as.numeric(exam$exam1[[i]]$metainfo$solution), collapse = "")
  solutions <- tibble(
    question = as.character(c(1:questnbr)),
    question_id = questions,
    points = points,
    solution = solutions
  ) %>%
    dplyr::mutate(right_answer = purrr::map(solution, function(x) c("a", "b", "c", "d", "e")[as.logical(as.numeric(unlist(strsplit(x, split = ""))))]))

  solutions <- solutions %>%
    dplyr::mutate(difficulty = round(0.2 + 4 * runif(nrow(solutions)) / 5, 1))

  student_level <- rnorm(nbr) / 8

  participants <- tibble(
    student_id = round(sample(unique(1000000 + runif(1000) * 1000000), length(student_level)), 0),
    level = student_level
  ) %>%
    dplyr::mutate(student_id = as.character(student_id))

  for (i in 1:length(student_level)) {
    proba <- student_level[[i]] + solutions$difficulty
    hurdle <- runif(length(proba)) * 0.75
    answers <- tibble(
      question_id = solutions$question_id,
      points = solutions$points,
      correct = proba >= hurdle,
      solution = unlist(solutions$right_answer)
    ) %>%
      dplyr::mutate(wrong = purrr::map(solution, function(x) sample(setdiff(c("a", "b", "c", "d", "e", ""), x), 1))) %>%
      dplyr::mutate(wrong = unlist(wrong)) %>%
      dplyr::mutate(
        answer = case_when(
          correct == TRUE ~ solution,
          TRUE ~ wrong
        )
      ) %>%
      dplyr::mutate(
        factor = case_when(
          answer == "" ~ 0,
          solution == answer ~ 1,
          TRUE ~ 0 - correction
        )
      ) %>%
      dplyr::mutate(score = factor * points) %>%
      dplyr::select(question_id, answer, score)

    names(answers) <- c(
      "question_id",
      paste0("answer_", participants$student_id[[i]]),
      paste0("score_", participants$student_id[[i]])
    )

    solutions <- dplyr::left_join(solutions, answers, by = "question_id")
  }

  answers <- solutions %>%
    dplyr::select(question, starts_with("answer_"), starts_with("score_")) %>%
    tidyr::gather(tosplit, value, -question) %>%
    tidyr::separate(tosplit, into = c("type", "student_id"), sep = "_") %>%
    tidyr::spread(type, value) %>%
    dplyr::mutate(score = as.numeric(score), question = as.integer(question))

  grades <- answers %>%
    dplyr::select(student_id, score) %>%
    dplyr::group_by(student_id) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::ungroup()

  results <- list(
    answers = answers,
    grades = grades
  )

  return(results)
}
