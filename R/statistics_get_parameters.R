#' @name statistics_get_parameters
#' @title Compute IRT statistics
#' @author Nicolas Mangin
#' @description Compute Item Response Theory statistics about documents and items.
#' @param results Tibble. observation (e.g. student attempt in test), code (e.g. question) and score (percentage of completion).
#' @param model_formula Character Model used to estimate parameters (converted into a formula in yhe function)
#' @param minobs Integer.
#' @return Models and parameters
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr ntile
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom psych fa
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr map_dbl
#' @importFrom purrr map_int
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @export


statistics_get_parameters <- function(
  results,
  model_formula = "correct ~ ability",
  minobs = 10
){
  
  observation <- NULL
  correct <- NULL
  data <- NULL
  model <- NULL
  probabilities <- NULL
  nbrobs <- NULL
  parameters <- NULL
  code <- NULL
  ability <- NULL
  difficulty <- NULL
  merit <- NULL
  score <- NULL
  range_ability <- NULL
  range_correct <- NULL
  
  naive_difficulty <- results |> 
    dplyr::group_by(code) |>
    dplyr::summarise(
      difficulty = 1 - base::mean(score),
      .groups = "drop"
    )
  
  naive_ability <- results |> 
    dplyr::left_join(naive_difficulty, by = "code") |>
    dplyr::mutate(merit = score * difficulty) |>
    dplyr::group_by(observation) |>
    dplyr::summarise(
      score = base::mean(score),
      merit = base::mean(merit),
      .groups = "drop"
    ) |>
    dplyr::mutate_if(base::is.numeric, function(x) base::as.numeric(base::scale(x)))
  naive_ability$ability <- psych::fa(naive_ability[,c(2:3)])$scores |>
    base::as.numeric() |>
    dplyr::ntile(100)
  
  characteristics <- results |>
    dplyr::group_by(code) |>
    dplyr::mutate(correct = base::as.numeric(score > 0.5)) |>
    dplyr::ungroup() |>
    dplyr::left_join(dplyr::select(naive_ability, observation, ability), by = "observation") |>
    dplyr::left_join(dplyr::select(naive_difficulty, code), by = c("code")) |>
    dplyr::group_by(code) |>
    tidyr::nest() |>
    dplyr::mutate(
      range_correct = purrr::map_dbl(data, function(x) base::max(x$correct) - base::min(x$correct)),
      range_ability = purrr::map_dbl(data, function(x) base::max(x$ability) - base::min(x$ability))
    ) |>
    dplyr::filter(range_correct > 0, range_ability > 0) |>
    dplyr::select(-range_correct,-range_ability) |>
    dplyr::mutate(
      model = purrr::map(
        data,
        function(x) base::suppressMessages(base::suppressWarnings(
          stats::glm(
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
