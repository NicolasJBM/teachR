#' @name update_tags
#' @title Update tags
#' @author Nicolas Mangin
#' @description Function taking all the tags listed in documents, adding the new ones to the existing list of tags, counting their occurrences, and saving the updated tags database on disk.
#' @param course_paths List. Paths to the different folders and databases on local disk.
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr summarise
#' @importFrom tidyr unnest
#' @importFrom purrr map
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
  data <- NULL
  
  if (base::file.exists(course_paths$databases$tags)){
    base::load(course_paths$databases$tags)
  } else {
    tags <- tibble::tibble(
      tag = base::factor(0),
      order = base::numeric(0),
      value = base::character(0),
      filter = base::character(0),
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
    tidyr::replace_na(base::list(value = "NA")) |>
    dplyr::mutate(value = purrr::map(value, function(x){
      tibble::tibble(value = base::as.character(stringr::str_split(x, pattern = " ", simplify = TRUE)))
    })) |>
    tidyr::unnest(value) |>
    dplyr::mutate(count = 1) |>
    dplyr::group_by(tag,value) |>
    dplyr::summarise(count = base::sum(count), .groups = "drop") |>
    base::suppressMessages() |>
    dplyr::mutate(
      order = 0,
      filter = "pattern",
      icon = "exclamation-triangle",
      boxcolor = "black"
    )
  
  newtags <- dplyr::anti_join(
    usedtags, tags, by = c("tag","value")
  )
  
  unusedtags <-  dplyr::anti_join(
    tags, usedtags, by = c("tag","value")
  ) |>
    dplyr::mutate(count = 0)
  
  updatedtags <- dplyr::inner_join(
    dplyr::select(tags, tag, order, value, filter, icon, boxcolor),
    dplyr::select(usedtags, tag, value, count),
    by = c("tag","value")
  )
  
  new_tags <- dplyr::bind_rows(newtags, unusedtags, updatedtags) |>
    dplyr::select(tag, order, value, filter, count, icon, boxcolor) |>
    dplyr::arrange(tag, order)

  tags <- new_tags |>
    dplyr::arrange(tag, order) |>
    dplyr::group_by(tag) |>
    tidyr::nest() |>
    dplyr::mutate(data = purrr::map(data, function(x){
      filt <- base::names(base::sort(base::table(x$filter), decreasing = TRUE)[1])
      col <- base::names(base::sort(base::table(x$boxcolor), decreasing = TRUE)[1])
      x$filter <- filt
      x$boxcolor <- col
      x$icon <- base::replace(x$icon, base::is.na(x$icon), "triangle-exclamation")
      x$icon <- base::replace(x$icon, x$icon == "", "triangle-exclamation")
      x
    })) |>
    tidyr::unnest(data) |>
    dplyr::ungroup()
  
  base::save(tags, file = course_paths$databases$tags)

}

