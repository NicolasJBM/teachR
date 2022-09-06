#' @name statistics_get_parameters
#' @title Compute various statistics
#' @author Nicolas Mangin
#' @description Compute various statistics
#' @param results Tibble. observation (e.g. student attempt in test), code (e.g. question) and whether the answer is correct.
#' @param model_formula Character Model used to estimate parameters (converted into a formula in yhe function)
#' @param minobs Integer.
#' @return Models and parameters
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom purrr map
#' @importFrom purrr map_int
#' @importFrom purrr map2
#' @importFrom stats as.formula
#' @importFrom stats glm
#' @importFrom stats predict
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @export


statistics_get_parameters <- function(
  results,
  model_formula = "correct ~ success + proficiency",
  minobs = 10
){
  
  observation <- NULL
  correct <- NULL
  success <- NULL
  data <- NULL
  model <- NULL
  probabilities <- NULL
  nbrobs <- NULL
  parameters <- NULL
  code <- NULL
  
  naive_proficiency <- results |>
    dplyr::group_by(observation) |>
    dplyr::summarise(
      proficiency = base::mean(correct, na.rm = TRUE), .groups = "drop"
    )
  
  naive_difficulty <- results |>
    dplyr::group_by(code) |>
    dplyr::summarise(
      success = base::mean(correct, na.rm = TRUE), .groups = "drop"
    ) |>
    dplyr::mutate(success = base::round(success, 2))
  
  characteristics <- results |>
    dplyr::left_join(naive_proficiency, by = "observation") |>
    dplyr::left_join(naive_difficulty, by = c("code")) |>
    dplyr::group_by(code) |>
    tidyr::nest() |>
    dplyr::mutate(
      model = purrr::map(
        data,
        function(x) base::suppressMessages(base::suppressWarnings(
          stats::glm(
            # add success rate to to model to account for documents with
            # multiple questions of different difficulty levels
            stats::as.formula(model_formula),
            data = x,
            family = "binomial"
          )
        ))
      )
    ) |>
    dplyr::mutate(
      probabilities = purrr::map2(
        model,
        data,
        function(x, y) base::suppressMessages(base::suppressWarnings(
          stats::predict(x, y, type = "response")
        ))
      )
    ) |>
    dplyr::mutate(
      data = purrr::map2(
        data,
        probabilities,
        function(x, y) dplyr::mutate(
          x,
          probability = y,
          prediction = base::ifelse(y > 0.5, 1, 0)
        )
      )
    ) |>
    dplyr::mutate(nbrobs = purrr::map_int(data, nrow)) |>
    dplyr::filter(nbrobs >= minobs) |>
    dplyr::mutate(
      parameters = purrr::map(data, teachR::statistics_compute_irt_parameters)
    ) |>
    dplyr::ungroup()
  
  stat_models <- characteristics |>
    dplyr::select(code, data, model)
  
  stat_parameters <- characteristics |>
    dplyr::select(code, parameters) |>
    tidyr::unnest(parameters)
  
  statistics <- base::list(
    models = stat_models,
    parameters = stat_parameters
  )
  
  return(statistics)
}
