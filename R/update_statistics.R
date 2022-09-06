#' @name update_statistics
#' @title Compte statistics for documents and items.
#' @author Nicolas Mangin
#' @description Compte statistics for documents and items.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param minobs Integer.
#' @return Aggregates, models, and parameters saved on local folders.
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr n
#' @importFrom dplyr rename
#' @importFrom dplyr sample_n
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom stats as.formula
#' @importFrom stats na.omit
#' @importFrom stats sd
#' @importFrom stringr str_replace_all
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @importFrom tidyr unite
#' @export


update_statistics <- function(course_paths, minobs = 10){
  
  attempt <- NULL
  average <- NULL
  checked <- NULL
  code <- NULL
  comments <- NULL
  correct <- NULL
  dispersion <- NULL
  document <- NULL
  documents <- NULL
  duration <- NULL
  earned <- NULL
  file_alt <- NULL
  item <- NULL
  language <- NULL
  observation <- NULL
  points <- NULL
  question <- NULL
  rate <- NULL
  rates <- NULL
  ratings <- NULL
  repetition <- NULL
  retention <- NULL
  propositions <- NULL
  student <- NULL
  tag_youtube <- NULL
  test <- NULL
  viewers <- NULL
  watchtime <- NULL
  weight <- NULL
  
  
  shinybusy::show_modal_spinner(
    spin = "orbit",
    text = "Please wait while the application updates statistics"
  )
  
  base::load(course_paths$databases$documents)
  
  # Ratings
  
  base::load(course_paths$databases$ratings)
  
  if (base::nrow(ratings) > 0){
    
    page_ratings <- ratings |>
      dplyr::group_by(file, code, language) |>
      dplyr::summarise(
        rates = dplyr::n(),
        average = base::mean(rate),
        dispersion = stats::sd(rate),
        .groups = "drop"
      ) |>
      dplyr::mutate_if(is.numeric, base::round, digits = 2) |>
      dplyr::select(file, rates, average, dispersion
      )
  } else {
    page_ratings <- tibble::tibble(
      file = base::character(0),
      rates = base::numeric(0),
      average = base::numeric(0),
      dispersion = base::numeric(0)
    )
  }
  
  page_ratings <- page_ratings |>
    teachR::statistics_assign_colors(type = "ratings")
  base::rm(ratings)
  
  
  # Comments
  
  base::load(course_paths$databases$comments)
  
  if (base::nrow(comments) > 0){
    page_comments <- comments |>
      dplyr::select(file, comment) |>
      base::unique()
  } else {
    page_comments <- tibble::tibble(
      file = base::character(0),
      comment = base::character(0)
    )
  }
  base::rm(comments)
  
  
  # Views
  
  base::load(course_paths$databases$views)
  
  if (base::nrow(views) > 0){
    views <- views |>
      dplyr::group_by(file, code, language) |>
      dplyr::summarise(
        views = base::sum(views),
        viewers = base::sum(viewers),
        watchtime = base::sum(watchtime),
        retention = base::sum(retention * views) / base::sum(views),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        duration = 60*(100 * (watchtime / retention)) / views,
        repetition = views / viewers
      ) |>
      dplyr::mutate_if(is.numeric, base::round, digits = 2) |>
      dplyr::select(
        file, views, viewers, watchtime, retention, duration, repetition
      )
    
    video2doc <- documents |>
      dplyr::filter(!base::is.na(tag_youtube) & base::nchar(tag_youtube) > 0) |>
      dplyr::mutate(file_alt = base::paste0(document, "_", language, ".Rmd")) |>
      dplyr::select(file, file_alt) |>
      stats::na.omit()
    views2 <- views |>
      dplyr::left_join(video2doc, by = "file") |>
      dplyr::select(-file) |>
      dplyr::rename(file = file_alt) |>
      dplyr::select(file, dplyr::everything())
    
    video_views <- dplyr::bind_rows(views, views2) |>
      stats::na.omit() |>
      dplyr::group_by(file) |>
      dplyr::sample_n(1) |>
      dplyr::ungroup()
    
    base::rm(video2doc, views2)
    
  } else {
    video_views <- tibble::tibble(
      file = base::character(0),
      views = base::numeric(0),
      viewers = base::numeric(0),
      watchtime = base::numeric(0),
      retention = base::numeric(0),
      duration = base::numeric(0),
      repetition = base::numeric(0)
    )
  }
  
  video_views <- video_views |>
    teachR::statistics_assign_colors(type = "videos")
  base::rm(views)
  
  
  # Results
  
  base::load(course_paths$databases$propositions)
  base::load(course_paths$databases$results)
  
  if (base::nrow(results) > 0){
    
    results <- results |>
      dplyr::left_join(dplyr::select(
        propositions, item, code = document
      ), by = c("item")) |>
      dplyr::select(
        test, student, attempt, file = question, code, item, language,
        points, checked, weight, earned
      )
    
    questions_irt <- results |>
      dplyr::filter(checked == 1) |>
      dplyr::select(
        test, student, attempt, code = file, points, earned
      ) |>
      stats::na.omit() |>
      tidyr::unite("observation", student, attempt, sep = "-") |>
      tidyr::unite("observation", test, observation, sep = ".") |>
      dplyr::select(observation, code, points, earned) |>
      dplyr::group_by(observation, code) |>
      dplyr::summarise(
        points = base::max(points),
        earned = base::sum(earned),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        correct = base::as.numeric(earned > 0.5 * points)
      ) |>
      dplyr::select(observation, code, correct) |>
      teachR::statistics_get_parameters(
        model_formula = "correct ~ success + proficiency",
        minobs = minobs
      )
    
    documents_irt <- results |>
      dplyr::filter(checked == 1) |>
      dplyr::select(
        test, student, attempt, code, language, points, earned
      ) |>
      stats::na.omit() |>
      tidyr::unite("observation", student, attempt, sep = "-") |>
      tidyr::unite("observation", test, observation, sep = ".") |>
      dplyr::mutate(code = base::paste0(code, "_", language, ".Rmd")) |>
      dplyr::select(observation, code, points, earned) |>
      dplyr::group_by(observation, code) |>
      dplyr::summarise(
        points = base::max(points),
        earned = base::sum(earned),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        correct = base::as.numeric(earned > 0.5 * points)
      ) |>
      dplyr::select(observation, code, correct) |>
      teachR::statistics_get_parameters(
        model_formula = "correct ~ success + proficiency",
        minobs = minobs
      )
    
    items_irt <- results |>
      dplyr::select(
        test, student, attempt, item, language, checked, weight, earned
      ) |>
      dplyr::mutate(item = stringr::str_replace_all(item, "_", "\\.")) |>
      stats::na.omit() |>
      tidyr::unite("observation", student, attempt, sep = "-") |>
      tidyr::unite("observation", test, observation, sep = ".") |>
      tidyr::unite("code", item, language, sep = "_") |>
      dplyr::select(observation, code, checked, weight, earned) |>
      dplyr::mutate(
        correct = dplyr::case_when(
          checked == 0 & weight <= 0 ~ 1,
          checked == 1 & weight > 0 ~ 1,
          TRUE ~ 0
        )
      ) |>
      dplyr::select(observation, code, correct) |>
      teachR::statistics_get_parameters(
        model = stats::as.formula("correct ~ success + proficiency"),
        minobs = minobs
      )
    
    document_parameters <- dplyr::bind_rows(questions_irt$parameters, documents_irt$parameters) |>
      dplyr::rename(file = code)
    document_models <- dplyr::bind_rows(questions_irt$models, documents_irt$models) |>
      dplyr::rename(file = code)
    item_parameters <- items_irt$parameters |>
      tidyr::separate(code, into = c("item","language"), sep = "_") |>
      dplyr::mutate(item = stringr::str_replace_all(item, "\\.", "_"))
    item_models <- items_irt$models |>
      tidyr::separate(code, into = c("item","language"), sep = "_") |>
      dplyr::mutate(item = stringr::str_replace_all(item, "\\.", "_"))
    
    base::rm(questions_irt, documents_irt, items_irt)
    
  } else {
    
    document_parameters <- tibble::tibble(
      file = base::character(0),
      answers = base::numeric(0),
      success = base::numeric(0),
      difficulty = base::numeric(0),
      discrimination = base::numeric(0),
      guess = base::numeric(0),
      accuracy = base::numeric(0)
    )
    
    document_models <- tibble::tibble(
      file = base::character(0),
      data = base::list(),
      model = base::list()
    )
    
    item_parameters <- tibble::tibble(
      item = base::character(0),
      language = base::character(0),
      answers = base::numeric(0),
      success = base::numeric(0),
      difficulty = base::numeric(0),
      discrimination = base::numeric(0),
      guess = base::numeric(0),
      accuracy = base::numeric(0)
    )
    
    item_models <- tibble::tibble(
      item = base::character(0),
      language = base::character(0),
      data = base::list(),
      model = base::list()
    )
    
  }
  
  document_parameters <- document_parameters |>
    teachR::statistics_assign_colors(type = "questions")
  item_parameters <- item_parameters |>
    teachR::statistics_assign_colors(type = "questions")
  base::rm(documents, propositions, results)
  
  
  
  base::save(page_ratings, file = course_paths$databases$page_ratings)
  base::save(page_comments, file = course_paths$databases$page_comments)
  base::save(video_views, file = course_paths$databases$video_views)
  base::save(document_parameters, file = course_paths$databases$document_parameters)
  base::save(document_models, file = course_paths$databases$document_models)
  base::save(item_parameters, file = course_paths$databases$item_parameters)
  base::save(item_models, file = course_paths$databases$item_models)
  
  
  
  shinybusy::remove_modal_spinner()
  
  shinyalert::shinyalert(
    title = "Statistics updated!",
    text = "Your statistics are now updated. Load the course to apply changes.",
    type = "success"
  )
}
