#' @name update_trees
#' @title Update course trees
#' @author Nicolas Mangin
#' @description Function adding missing documents as unclassified and removing non-existing documents in all trees.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Save on disk updated trees.
#' @importFrom classR trees_tibble_to_json
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map_chr
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom tibble rowid_to_column
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na
#' @export


update_trees <- function(course_paths){

  title <- NULL
  position <- NULL
  description <- NULL
  text <- NULL
  documents <- NULL
  document_types <- NULL
  
  
  # Load necessary data
  base::load(course_paths$databases$documents)
  base::load(course_paths$databases$doctypes)
  
  if (!base::file.exists(course_paths$databases$courses)){
    courses <- tibble::tibble(
      tree = "unclassified.RData",
      course = base::as.character(NA),
      authors = base::as.character(NA),
      institution = base::as.character(NA),
      program = base::as.character(NA),
      program_level = base::as.character(NA),
      group = base::as.character(NA),
      year = base::as.character(NA),
      website = base::as.character(NA)
    )
    base::save(courses, file = course_paths$databases$courses)
  }
  
  # Prepare the tree structure
  preptree1 <- tibble::tibble(
    position = c("1.0.0","2.0.0","3.0.0"),
    text = c("Included","Excluded","Unclassified")
  )

  # Prepare a tree tibble where all documents are unclassified
  preptree2 <- documents |>
    dplyr::mutate(
      text = title,
      position = 1:base::nrow(documents)
    ) |>
    dplyr::mutate(position = base::paste0("3.", position, ".0"))

  # Retrieve document types to get related icons and box colors
  preptree3 <- dplyr::select(document_types, -description)

  # Make a tree tibble where all documents are unclassified
  tree <- dplyr::bind_rows(preptree1, preptree2) |>
    dplyr::left_join(preptree3, by = "type") |>
    tidyr::replace_na(base::list(icon = "folder-open"))

  # Save the unclassified tree (typically to start classifications from scratch)
  base::save(
    tree,
    file = base::paste0(course_paths$subfolders$trees, "/unclassified.RData")
  )

  jstree <- tree |>
    classR::trees_tibble_to_json()
  
  base::save(
    jstree,
    file = base::paste0(course_paths$subfolders$jstrees, "/unclassified.RData")
  )
  
  # Identify the other trees in the course
  other_trees <- base::setdiff(
    base::list.files(course_paths$subfolders$trees),
    "unclassified.RData"
  )

  # For each tree, add missing documents as unclassified and
  # remove non-existing documents.
  for (classif in other_trees){

    base::load(base::paste0(course_paths$subfolders$trees, "/", classif))

    classified <- tree |>
      dplyr::filter(
        !stringr::str_detect(position, "^3.[1-9]"),
        file %in% documents$file | base::is.na(file)
      ) |>
      dplyr::select(position, file, text) |>
      dplyr::left_join(documents, by = "file") |>
      dplyr::select(position, text, dplyr::everything())

    levels <- base::nchar(
      stringr::str_remove_all(classified$position[1], "\\.")
    )

    add <- documents |>
      dplyr::filter(!(file %in% classified$file)) |>
      tibble::rowid_to_column("position") |>
      dplyr::mutate(position = purrr::map_chr(position, function(x, levels){
        base::paste(c(3,x,base::rep(0,(levels-2))), collapse = ".")
      }, levels)) |>
      dplyr::mutate(text = "")

    tree <- dplyr::bind_rows(classified, add) |>
      dplyr::mutate(text = dplyr::case_when(
        base::is.na(title) ~ text,
        TRUE ~ title
      )) |>
      dplyr::left_join(document_types, by = "type") |>
      dplyr::mutate(
        icon = dplyr::case_when(
          base::is.na(icon) ~ "folder-open",
          TRUE ~ icon
        ),
        boxcolor = dplyr::case_when(
          base::is.na(boxcolor) ~ "black",
          TRUE ~ boxcolor
        )
      ) |>
      dplyr::select(position, text, dplyr::everything())
    
    base::save(
      tree,
      file = base::paste0(course_paths$subfolders$trees, "/", classif)
    )
    
    jstree <- tree |>
      classR::trees_tibble_to_json()
    
    base::save(
      jstree,
      file = base::paste0(course_paths$subfolders$jstrees, "/", classif)
    )
  }
  
}
