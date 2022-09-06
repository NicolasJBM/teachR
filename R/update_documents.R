#' @name update_documents
#' @title Update document list
#' @author Nicolas Mangin
#' @param course_paths List. Paths to the different folders and databases on local disk.
#' @description Function listing all the documents, checking whether they were changed, and updating information about changed documents.
#' @return Save the document list as a tibble in the folder "2_documents" and returns this list
#' @importFrom dplyr anti_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr group_by
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom editR get_tags
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom stringr str_remove_all
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @importFrom tidyr replace_na
#' @importFrom tidyr unnest
#' @export

update_documents <- function(course_paths){
  
  path <- NA
  modified <- NA
  language <- NA
  tags <- NA
  code <- NA
  
  shinybusy::show_modal_spinner(
    spin = "orbit",
    text = "Please wait while the application updates documents..."
  )
  
  if (!base::file.exists(course_paths$databases$documents)){
    documents <- base::data.frame(
      file = base::character(0),
      code = base::character(0),
      language = base::character(0),
      translations = base::character(0),
      path = base::character(0),
      modified = base::character(0),
      title = base::character(0),
      type = base::character(0),
      document = base::character(0),
      tag_content = base::character(0),
      tag_level = base::character(0),
      tag_bloom = base::character(0),
      tag_custom = base::character(0),
      tag_authors = base::character(0),
      tag_youtube = base::character(0)
    )
    documents$modified <- base::as.Date(documents$modified)
    base::save(documents, file = course_paths$databases$documents)
  } else {
    base::load(course_paths$databases$documents)
  }
  
  file_info <- tibble::tibble(
    path = base::list.files(course_paths$subfolders$original, full.names = TRUE)
  ) |>
    dplyr::mutate(
      file = stringr::str_remove_all(path, base::paste0(course_paths$subfolders$original, "/")),
      modified = purrr::map(path, function(x) base::file.info(x)$mtime)
    ) |>
    tidyr::unnest(modified)
  
  documents <-  dplyr::select(documents, -translations)
  
  unchanged <- file_info |>
    dplyr::inner_join(documents, by = c("file","modified"))
  
  changed <- file_info |>
    dplyr::anti_join(documents, by = c("file","modified"))
  
  if (base::nrow(changed) > 0){
    documents <- changed |>
      tidyr::separate(
        file, into = c("code","language"), sep = "_", remove = FALSE
      ) |>
      dplyr::mutate(
        language = stringr::str_remove_all(language, ".Rmd"),
        tags = purrr::map(path, editR::get_tags)
      ) |>
      tidyr::unnest(tags) |>
      dplyr::bind_rows(unchanged)
  } else {
    documents <- unchanged
  }
  
  translations <- tibble::tibble(
    file = base::list.files(course_paths$subfolders$translated)
  ) |>
    tidyr::separate(
    file, into = c("code","language"), sep = "_", remove = TRUE
  ) |>
    dplyr::mutate(
      language = stringr::str_remove_all(language, ".Rmd")
    ) |>
    dplyr::group_by(code) |>
    dplyr::summarise(translations = base::paste(language, collapse = " "))
  
  documents <- dplyr::left_join(documents, translations, by = "code") |>
    tidyr::replace_na(base::list(translations = "")) |>
    dplyr::select(file, code, language, translations, dplyr::everything()) |>
    dplyr::select(-path)
  
  base::save(documents, file = course_paths$databases$documents)
  
  shinybusy::remove_modal_spinner()
  
  shinyalert::shinyalert(
    title = "Documents updated!",
    text = "Your documents are now updated. Load the course to apply changes.",
    type = "success"
  )
}

