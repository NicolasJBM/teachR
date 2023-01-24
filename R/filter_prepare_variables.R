#' @name filter_prepare_variables
#' @title Prepare a list of variables for filters
#' @author Nicolas Mangin
#' @description Function creating the variable list serving as input to the function making filters.
#' @param preselection Tibble. List of remaining documents after some prior selection.
#' @param filter_family Character. Whether the filtering step is for "common_tags" "custom_tags", "ratings", "views", or "results".
#' @param tags Tittle. Tags definitions.
#' @return A tibble indicating the variables for which filters should be created, the associated input id, and the format of the filter. Used as input for filter_make_ui.
#' @seealso filter_make_ui
#' @seealso filter_tibble
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr sample_n
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr ungroup
#' @importFrom tibble tibble
#' @export


filter_prepare_variables <- function(preselection, filter_family, tags){
  
  variable_name <- NULL
  
  if (filter_family == "common_tags"){
    
    common_tags <- c("title","authors","document","type")
    filter_variables <- tibble::tibble(
      variable_name = common_tags,
      input_id = base::paste0("slctfilt", common_tags),
      filter_type = c("pattern","pattern","multiple","multiple")
    )
    
  } else if (filter_family == "custom_tags") {
    
    custom_tags <- preselection |>
      dplyr::select(dplyr::starts_with("tag_")) |>
      base::names()
    custom_tags <- base::setdiff(custom_tags, "tag_custom")
    
    filters <- dplyr::select(tags, variable_name = tag, filter_type = filter) |>
      dplyr::group_by(variable_name) |>
      dplyr::sample_n(1) |>
      dplyr::ungroup()
    
    filter_variables <- tibble::tibble(
      variable_name = custom_tags,
      input_id = base::paste0("slctfilt", custom_tags)
    ) |>
      dplyr::left_join(filters, by = "variable_name")
    
  } else if (filter_family == "ratings") {
    
    ratings_var <- c("rates","average","dispersion")
    filter_variables <- tibble::tibble(
      variable_name = ratings_var,
      input_id = base::paste0("slctfilt", ratings_var),
      filter_type = base::rep("range", 3)
    )
    
  } else if (filter_family == "views") {
    
    views_var <- c("viewers","views","repetition","retention")
    filter_variables <- tibble::tibble(
      variable_name = views_var,
      input_id = base::paste0("slctfilt", views_var),
      filter_type = base::rep("range", 4)
    )
    
  } else { # if results
    
    results_var <- c("success","difficulty","discrimination","guess","accuracy")
    filter_variables <- tibble::tibble(
      variable_name = results_var,
      input_id = base::paste0("slctfilt", results_var),
      filter_type = base::rep("range", 5)
    )
    
  }
  
  return(filter_variables)
}


