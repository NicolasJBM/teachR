#' @name update_trees
#' @title Update course trees
#' @author Nicolas Mangin
#' @description Function adding missing documents as unclassified, removing non-existing documents in all trees, and saving changes on disk.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @importFrom classR tbltree_to_jstree
#' @importFrom classR complete_position
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map_chr
#' @importFrom tibble rowid_to_column
#' @importFrom tibble tibble
#' @export


update_trees <- function(course_paths){

  title <- NULL
  position <- NULL
  description <- NULL
  text <- NULL
  documents <- NULL
  document_types <- NULL
  active <- NULL
  path <- NULL
  tmp <- NULL
  translations <- NULL
  type <- NULL
  
  
  # Load necessary data
  base::load(course_paths$databases$documents)
  base::load(course_paths$databases$doctypes)
  
  tbltree <- documents |>
    dplyr::select(file, title, type, translations) |>
    dplyr::left_join(dplyr::select(document_types, type, icon), by = "type") |>
    tibble::rowid_to_column("tmp") |>
    dplyr::mutate(tmp = purrr::map_chr(tmp, classR::complete_position, 9)) |>
    dplyr::mutate(
      path = base::paste0("Unclassified/", file),
      position = base::paste0("03-", tmp)
    ) |>
    dplyr::select(file, path, position, title, type, icon, translations)  |>
    dplyr::arrange(position)
  base::save(tbltree, file = base::paste0(course_paths$subfolders$tbltrees, "/unclassified.RData"))
  
  jstree <- classR::tbltree_to_jstree(tbltree)
  
  base::save(jstree, file = base::paste0(course_paths$subfolders$jstrees, "/unclassified.RData"))
  
  tbltree <- NA
  jstree <- NA
  
  alltrees <- base::list.files(course_paths$subfolders$jstrees)
  
  for (slcttree in alltrees){
    
    base::load(base::paste0(course_paths$subfolders$tbltrees, "/", slcttree))
    
    newtbltree <- documents |>
      dplyr::select(file, title, type, translations) |>
      dplyr::left_join(dplyr::select(document_types, type, icon), by = "type") |>
      dplyr::left_join(dplyr::select(tbltree, file, path, position), by = "file") |>
      tibble::rowid_to_column("tmp") |>
      dplyr::mutate(tmp = purrr::map_chr(tmp, classR::complete_position, 9)) |>
      dplyr::mutate(
        path = dplyr::case_when(
          base::is.na(path) ~ base::paste0("Unclassified/", file),
          TRUE ~ path
        ),
        position = dplyr::case_when(
          base::is.na(position) ~ paste0("03-", tmp),
          TRUE ~ position
        )
      ) |>
      dplyr::select(file, path, position, title, type, icon, translations)  |>
      dplyr::arrange(position)
    
    tbltree <- newtbltree
    base::save(tbltree, file = base::paste0(course_paths$subfolders$tbltrees, "/", slcttree))
    
    jstree <- classR::tbltree_to_jstree(tbltree)
    base::save(jstree, file = base::paste0(course_paths$subfolders$jstrees, "/", slcttree))
  }
  
}
