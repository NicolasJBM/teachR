#' @name statistics_compute_irt_parameters
#' @title Compite IRT parameters
#' @author Nicolas Mangin
#' @description Compute difficulty, discrimination, and guess parameters
#' @param predictions Tibbles.
#' @return Tibble.
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr lag
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr sample_n
#' @importFrom dplyr select
#' @export


statistics_compute_irt_parameters <- function(predictions){
  
  proficiency <- NULL
  probability <- NULL
  correct <- NULL
  prediction <- NULL
  lag_prob <- NULL
  lag_prof <- NULL
  slope <- NULL
  accuracy <- NULL
  discrimination <- NULL
  answers <- NULL
  success <- NULL
  guess <- NULL
  
  predictions |>
    dplyr::arrange(proficiency) |>
    dplyr::mutate(
      lag_prof = dplyr::lag(proficiency),
      lag_prob = dplyr::lag(probability),
      accuracy = base::as.numeric(correct == prediction)
    ) |>
    dplyr::mutate(
      answers = base::length(probability),
      slope = (probability - lag_prob) / (proficiency - lag_prof)
    ) |>
    dplyr::mutate(
      discrimination = base::max(slope, na.rm = TRUE),
      guess = base::min(probability, na.rm = TRUE),
      accuracy = base::mean(accuracy, na.rm = TRUE)
    ) |>
    dplyr::filter(slope == discrimination) |>
    dplyr::sample_n(1) |>
    dplyr::select(
      answers,
      success,
      difficulty = proficiency,
      discrimination,
      guess,
      accuracy
    ) |>
    dplyr::mutate_all(function(x) base::round(x, 2))
}

