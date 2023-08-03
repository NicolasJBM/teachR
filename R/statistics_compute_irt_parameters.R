#' @name statistics_compute_irt_parameters
#' @title Compite IRT parameters
#' @author Nicolas Mangin
#' @description Compute difficulty, discrimination, and guess parameters about documents and items.
#' @param predictions Tibbles.
#' @return Tibble.
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_cols
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr summarise_all
#' @importFrom dplyr ungroup
#' @importFrom tibble tibble
#' @export


statistics_compute_irt_parameters <- function(predictions){
  
  ability <- NULL
  probability <- NULL
  correct <- NULL
  prediction <- NULL
  lag_prob <- NULL
  lag_abil <- NULL
  slope <- NULL
  accuracy <- NULL
  discrimination <- NULL
  answers <- NULL
  success <- NULL
  guess <- NULL
  difficulty <- NULL
  slope_deciles <- NULL
  
  basic_statistics <- predictions |>
    dplyr::ungroup() |>
    base::unique() |>
    dplyr::summarise(
      answers = dplyr::n(),
      success = base::round(base::mean(correct)*100,0),
      guess = base::round(base::min(probability, na.rm = TRUE)*100,0),
      accuracy = base::round(base::mean(base::as.numeric(correct == prediction))*100,0)
    )
  
  smooth_basis <- predictions |>
    dplyr::mutate(difficulty = ability / 10) |>
    dplyr::group_by(difficulty) |>
    dplyr::summarise(
      probability = base::mean(probability, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(difficulty) 
  
  smooth <- stats::loess.smooth(x = smooth_basis$difficulty, y = smooth_basis$probability)
  
  irt_statistics <- tibble::tibble(
    difficulty = smooth$x, probability = smooth$y,
    discrimination = c(0, base::diff(smooth$y)/base::diff(smooth$x))
  ) |>
    dplyr::filter(probability >= 0.45, probability <= 0.55) |>
    dplyr::summarise_all(base::mean, na.rm = TRUE) |>
    dplyr::mutate(
      difficulty = base::round(difficulty, 1),
      discrimination = base::round(discrimination*10, 1)
    ) |>
    base::unique()
  
  dplyr::bind_cols(basic_statistics, irt_statistics) |>
    dplyr::select(
      answers,
      success,
      difficulty,
      discrimination,
      guess,
      accuracy
    )
}

