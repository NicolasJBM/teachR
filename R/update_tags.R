#' @name update_tags
#' @title Update tags
#' @author Nicolas Mangin
#' @description Function taking all the tags listed in documents, adding the new ones to the existing list of tags, and counting their occurrences.
#' @param course_paths List. Paths to the different folders and databases on local disk.
#' @return Save the tag list as a tibble in the folder "2_documents" and returns this list
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr summarise
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr replace_na
#' @export

update_tags <- function(course_paths){
  
  tag <- NULL
  value <- NULL
  boxcolor <- NULL
  documents <- NULL
  count <- NULL
  
  if (base::file.exists(course_paths$databases$tags)){
    base::load(course_paths$databases$tags)
  } else {
    tags <- tibble::tibble(
      tag = base::factor(0),
      order = base::numeric(0),
      value = base::character(0),
      count = base::numeric(0),
      icon = base::character(0),
      boxcolor = base::character(0)
    )
  }
    
  base::load(course_paths$databases$documents)

  usedtags <- dplyr::select(documents, dplyr::starts_with("tag_"))
  
  usedtags <- usedtags |>
    tidyr::pivot_longer(
      cols = base::names(usedtags), names_to = "tag", values_to = "value"
    ) |>
    tidyr::replace_na(base::list(value = "")) |>
    dplyr::mutate(count = 1) |>
    dplyr::group_by(tag,value) |>
    dplyr::summarise(count = base::sum(count), .groups = "drop") |>
    base::suppressMessages() |>
    dplyr::mutate(
      order = 0, icon = "exclamation-triangle", boxcolor = "black"
    )
  
  newtags <- dplyr::anti_join(
    usedtags, tags, by = c("tag","value")
  )
  
  unusedtags <-  dplyr::anti_join(
    tags, usedtags, by = c("tag","value")
  ) |>
    dplyr::mutate(count = 0)
  
  updatedtags <- dplyr::inner_join(
    dplyr::select(tags, tag, order, value, icon, boxcolor),
    dplyr::select(usedtags, tag, value, count),
    by = c("tag","value")
  )
  
  new_tags <- dplyr::bind_rows(newtags, unusedtags, updatedtags) |>
    dplyr::select(tag, order, value, count, icon, boxcolor) |>
    dplyr::arrange(tag, order)

  tags <- new_tags |>
    dplyr::arrange(tag, order)
  
  base::save(tags, file = course_paths$databases$tags)

}

