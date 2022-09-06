#' @name filter_tibble
#' @title Filter tibble
#' @author Nicolas Mangin
#' @description Function applying different kinds of filters on a tibble. The procedure applied is determined by the filter type.
#' @param dataset Tibble. Data to be filtered.
#' @param variable Character. Variable used for filtering.
#' @param filter_value Character. Value to be filtered.
#' @param filter_type Character. Type of filter applied. One of the following: "logical" for a true or false, selection" for a single string value, "multiple" for multiple string values, "pattern" for a regular expression, "range" for a range of numeric values, "value", for a single value.
#' @return Tibble after application of the relevant filter.
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_split
#' @importFrom stringr str_replace_all
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @export


filter_tibble <- function(
  dataset,
  variable,
  filter_value = NULL,
  filter_type = "pattern"
) {
  
  base::stopifnot(
    filter_type %in% c(
      "logical", "selection", "multiple", "pattern", "range", "value"
    )
  )
  
  if (base::is.null(filter_value[1]) | base::is.na(filter_value[1]) |
      filter_value[1] == "" | base::nrow(dataset) == 0) {
    
    dataset
    
  } else {
    
    if (filter_type == "logical") {
      
      if (filter_value[1]){
        
        dplyr::filter(
          dataset,
          dataset[, variable] == filter_value[1]
        )
        
      } else dataset
      
    } else if (filter_type == "selection") {
      
      dplyr::filter(
        dataset,
        dataset[, variable] == filter_value
      )
      
    } else if (filter_type == "multiple") {
      
      rgx <- base::paste(filter_value, collapse = "|")
      dplyr::filter(
        dataset,
        stringr::str_detect(base::unlist(dataset[, variable]), rgx)
      )
      
    } else if (filter_type == "pattern") {
      
      filter_value <- stringr::str_remove_all(filter_value, "[()]")
      terms <- stringr::str_to_lower(
        base::unlist(
          stringr::str_split(
            filter_value,
            " "
          )
        )
      )
      terms <- stringr::str_replace_all(terms, "_", " ")
      base <- dataset
      for (term in terms) {
        base <- dplyr::filter(
          base,
          stringr::str_detect(
            stringr::str_to_lower(
              base::unlist(
                base[, variable]
              )
            ),
            term
          )
        )
      }
      base
      
    } else if (filter_type == "range") {
      
      dplyr::filter(
        dataset,
        dataset[, variable] >= filter_value[1],
        dataset[, variable] <= filter_value[2]
      )
      
    } else { # so if value
      
      if (!base::is.na(filter_value[1])){
        
        dplyr::filter(
          dataset,
          dataset[, variable] == filter_value[1]
        )
        
      } else dataset
      
    }
  }
}
