#' @name statistics_assign_colors
#' @title Assign colors.
#' @author Nicolas Mangin
#' @description Function assigning colors based on documents' statistics.
#' @param parameters Tibble. Page ratings.
#' @param type Character. Whether the parameterds are for "ratings", "videos", or "questions".
#' @return Parameters with variable indicating colors for value boxes.
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @export


statistics_assign_colors <- function(parameters, type = "questions"){
  
  if (type == "ratings"){
    parameters |>
      dplyr::mutate(
        rates_color = dplyr::case_when(
          base::is.na(rates) ~ "purple",
          rates <= stats::quantile(rates, 0.25, na.rm = TRUE) ~ "red",
          rates <= stats::quantile(rates, 0.50, na.rm = TRUE) ~ "orange",
          rates <= stats::quantile(rates, 0.75, na.rm = TRUE) ~ "olive",
          TRUE ~ "green"
        ),
        average_color = dplyr::case_when(
          base::is.na(average) ~ "purple",
          average <= stats::quantile(average, 0.25, na.rm = TRUE) ~ "red",
          average <= stats::quantile(average, 0.50, na.rm = TRUE) ~ "orange",
          average <= stats::quantile(average, 0.75, na.rm = TRUE) ~ "olive",
          TRUE ~ "green"
        ),
        dispersion_color = dplyr::case_when(
          base::is.na(dispersion) ~ "purple",
          dispersion > stats::quantile(dispersion, 0.75, na.rm = TRUE) ~ "red",
          dispersion > stats::quantile(dispersion, 0.50, na.rm = TRUE) ~ "orange",
          dispersion > stats::quantile(dispersion, 0.25, na.rm = TRUE) ~ "olive",
          TRUE ~ "green"
        )
      )
  } else if (type == "videos"){
    parameters |>
      dplyr::mutate(
        views_color = dplyr::case_when(
          base::is.na(views) ~ "purple",
          views <= stats::quantile(views, 0.25, na.rm = TRUE) ~ "red",
          views <= stats::quantile(views, 0.50, na.rm = TRUE) ~ "orange",
          views <= stats::quantile(views, 0.75, na.rm = TRUE) ~ "olive",
          TRUE ~ "green"
        ),
        viewers_color = dplyr::case_when(
          base::is.na(viewers) ~ "purple",
          viewers <= stats::quantile(viewers, 0.25, na.rm = TRUE) ~ "red",
          viewers <= stats::quantile(viewers, 0.50, na.rm = TRUE) ~ "orange",
          viewers <= stats::quantile(viewers, 0.75, na.rm = TRUE) ~ "olive",
          TRUE ~ "green"
        ),
        watchtime_color = dplyr::case_when(
          base::is.na(watchtime) ~ "purple",
          watchtime <= stats::quantile(watchtime, 0.75, na.rm = TRUE) ~ "red",
          watchtime <= stats::quantile(watchtime, 0.50, na.rm = TRUE) ~ "orange",
          watchtime <= stats::quantile(watchtime, 0.25, na.rm = TRUE) ~ "olive",
          TRUE ~ "green"
        ),
        retention_color = dplyr::case_when(
          base::is.na(retention) ~ "purple",
          retention <= stats::quantile(retention, 0.25, na.rm = TRUE) ~ "red",
          retention <= stats::quantile(retention, 0.50, na.rm = TRUE) ~ "orange",
          retention <= stats::quantile(retention, 0.75, na.rm = TRUE) ~ "olive",
          TRUE ~ "green"
        ),
        duration_color = dplyr::case_when(
          base::is.na(duration) ~ "purple",
          duration > stats::quantile(duration, 0.75, na.rm = TRUE) ~ "red",
          duration > stats::quantile(duration, 0.50, na.rm = TRUE) ~ "orange",
          duration > stats::quantile(duration, 0.25, na.rm = TRUE) ~ "olive",
          TRUE ~ "green"
        ),
        repetition_color = dplyr::case_when(
          base::is.na(repetition) ~ "purple",
          repetition > stats::quantile(repetition, 0.25, na.rm = TRUE) ~ "red",
          repetition > stats::quantile(repetition, 0.50, na.rm = TRUE) ~ "orange",
          repetition > stats::quantile(repetition, 0.75, na.rm = TRUE) ~ "olive",
          TRUE ~ "green"
        )
      )
  } else {
    parameters |>
      dplyr::mutate(
        answers_color = dplyr::case_when(
          base::is.na(answers) ~ "purple",
          answers <= stats::quantile(answers, 0.25, na.rm = TRUE) ~ "red",
          answers <= stats::quantile(answers, 0.50, na.rm = TRUE) ~ "orange",
          answers <= stats::quantile(answers, 0.75, na.rm = TRUE) ~ "olive",
          TRUE ~ "green"
        ),
        success_color = dplyr::case_when(
          base::is.na(success) ~ "purple",
          success <= stats::quantile(success, 0.25, na.rm = TRUE) ~ "red",
          success <= stats::quantile(success, 0.50, na.rm = TRUE) ~ "orange",
          success <= stats::quantile(success, 0.75, na.rm = TRUE) ~ "olive",
          TRUE ~ "green"
        ),
        difficulty_color = dplyr::case_when(
          base::is.na(difficulty) ~ "purple",
          difficulty > stats::quantile(difficulty, 0.75, na.rm = TRUE) ~ "red",
          difficulty > stats::quantile(difficulty, 0.50, na.rm = TRUE) ~ "orange",
          difficulty > stats::quantile(difficulty, 0.25, na.rm = TRUE) ~ "olive",
          TRUE ~ "green"
        ),
        discrimination_color = dplyr::case_when(
          base::is.na(discrimination) ~ "purple",
          discrimination <= stats::quantile(discrimination, 0.25, na.rm = TRUE) ~ "red",
          discrimination <= stats::quantile(discrimination, 0.50, na.rm = TRUE) ~ "orange",
          discrimination <= stats::quantile(discrimination, 0.75, na.rm = TRUE) ~ "olive",
          TRUE ~ "green"
        ),
        guess_color = dplyr::case_when(
          base::is.na(guess) ~ "purple",
          guess > stats::quantile(guess, 0.75, na.rm = TRUE) ~ "red",
          guess > stats::quantile(guess, 0.50, na.rm = TRUE) ~ "orange",
          guess > stats::quantile(guess, 0.25, na.rm = TRUE) ~ "olive",
          TRUE ~ "green"
        ),
        accuracy_color = dplyr::case_when(
          base::is.na(accuracy) ~ "purple",
          accuracy <= stats::quantile(accuracy, 0.25, na.rm = TRUE) ~ "red",
          accuracy <= stats::quantile(accuracy, 0.50, na.rm = TRUE) ~ "orange",
          accuracy <= stats::quantile(accuracy, 0.75, na.rm = TRUE) ~ "olive",
          TRUE ~ "green"
        )
      )
  }
  
  
}
