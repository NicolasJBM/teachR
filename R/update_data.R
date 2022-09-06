#' @name update_data
#' @title Update course tests
#' @author Nicolas Mangin
#' @description Function listing tests and gathering tests definition parameters in a single database.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Save on disk updated test database.
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr sample_n
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom purrr map
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom stats na.omit
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @importFrom tidyr unnest
#' @importFrom utils read.csv
#' @export

update_data <- function(course_paths){
  
  group <- NULL
  student <- NULL
  student_alt <- NULL
  test_folders <- NULL
  test_parameters <- NULL
  Content <- NULL
  Date <- NULL
  Views <- NULL
  folder <- NULL
  viewers <- NULL
  views <- NULL
  youtube <- NULL
  Average.percentage.viewed.... <- NULL
  Average.view.duration <- NULL
  Unique.viewers <- NULL
  Watch.time..hours. <- NULL
  code <- NULL
  documents <- NULL
  language <- NULL
  section <- NULL
  tag_youtube <- NULL

  shinybusy::show_modal_spinner(
    spin = "orbit",
    text = "Please wait while the application updates data..."
  )
  
  
  # Ratings
  ratings <- tibble::tibble(
    folder = base::list.files(
      course_paths$subfolders$ratings, recursive = FALSE, full.names = TRUE
    )
  ) |>
    dplyr::filter(!stringr::str_detect(folder, "archives$")) |>
    dplyr::mutate(
      ratings = purrr::map(folder, function(x){
        if (base::file.exists((x))) utils::read.csv(x)
      })
    ) |>
    dplyr::select(-folder) |>
    tidyr::unnest(ratings) |>
    tidyr::separate(section, into = c("group","file"), sep = "-", remove = TRUE) |>
    tidyr::separate(file, into = c("code","language"), sep = "_", remove = FALSE) |>
    dplyr::mutate(language = stringr::str_remove_all(language, ".Rmd$"))
  
  base::save(ratings, file = course_paths$databases$ratings)
  
  # Comments
  comments <- tibble::tibble(
    folder = base::list.files(
      course_paths$subfolders$comments, recursive = FALSE, full.names = TRUE
    )
  ) |>
    dplyr::filter(!stringr::str_detect(folder, "archives$")) |>
    dplyr::mutate(
      comments = purrr::map(folder, function(x){
        if (base::file.exists((x))) utils::read.csv(x)
      })
    ) |>
    dplyr::select(-folder) |>
    tidyr::unnest(comments) |>
    base::unique() |>
    tidyr::separate(section, into = c("group","file"), sep = "-", remove = TRUE) |>
    tidyr::separate(file, into = c("code","language"), sep = "_", remove = FALSE) |>
    dplyr::mutate(language = stringr::str_remove_all(language, ".Rmd$"))
  
  base::save(comments, file = course_paths$databases$comments)
  
  # Views
  
  views_data <- tibble::tibble(
    folder = base::list.files(
      course_paths$subfolders$views, recursive = FALSE, full.names = TRUE
    )
  ) |>
    dplyr::filter(!stringr::str_detect(folder, "archives$")) |>
    dplyr::mutate(
      views = purrr::map(folder, function(x){
        if (base::file.exists((x))) utils::read.csv(x)
      })
    ) |>
    dplyr::select(-folder) |>
    tidyr::unnest(views) |>
    dplyr::select(
      tag_youtube = Content,
      watchtime = Watch.time..hours.,
      views = Views,
      viewers = Unique.viewers,
      duration = Average.view.duration,
      retention = Average.percentage.viewed....
    ) |>
    dplyr::filter(tag_youtube != "Total")
  
  base::load(course_paths$databases$documents)
  
  views <- documents |>
    dplyr::select(tag_youtube, file, code, language) |>
    stats::na.omit() |>
    dplyr::left_join(views_data, by = "tag_youtube")
  
  base::save(views, file = course_paths$databases$views)
  
  # Tests, students, and results
  alltests <- tibble::tibble(
    test_folders = base::list.dirs(
      course_paths$subfolders$tests, recursive = FALSE, full.names = TRUE
    )
  )  |>
    dplyr::filter(!stringr::str_detect(test_folders, "archives$|default$")) |>
    dplyr::mutate(
      tests = purrr::map(test_folders, function(x){
        base::load(base::paste0(x,"/test_parameters.RData"))
        return(test_parameters)
      }),
      students = purrr::map(test_folders, function(x, y){
        file <- base::paste0(x,"/6_students/student_list.csv")
        if (base::file.exists((file))) utils::read.csv(file)
      }),
      results = purrr::map(test_folders, function(x, y){
        file <- base::paste0(x,"/8_results/results.csv")
        if (base::file.exists((file))) utils::read.csv(file)
      })
    )
  
  tests <- dplyr::select(alltests, tests) |>
    tidyr::unnest(tests)
  base::save(tests, file = course_paths$databases$tests)
  
  students_new <- dplyr::select(alltests, students) |>
    tidyr::unnest(students) |>
    dplyr::group_by(group, student) |>
    dplyr::sample_n(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(student_alt = NA) |>
    dplyr::select(group, student, student_alt, dplyr::everything()) |>
    dplyr::arrange(group, student)
  
  if (base::file.exists(course_paths$databases$students)){
    base::load(course_paths$databases$students)
    students_new <- students_new |>
      dplyr::anti_join(students, by = c("group","student"))
    students <- students |>
      dplyr::bind_rows(students_new)
  } else students <- students_new
    
  for (i in 1:base::nrow(students)){
    if (base::is.na(students$student_alt[i])){
      students$student_alt[i] <- i
    }
  }
  
  base::save(students, file = course_paths$databases$students)
  
  results <- dplyr::select(alltests, results) |>
    tidyr::unnest(results)
  base::save(results, file = course_paths$databases$results)
  
  shinybusy::remove_modal_spinner()
  
  shinyalert::shinyalert(
    title = "Data updated!",
    text = "Your data are now updated. Load the course to apply changes.",
    type = "success"
  )

}
