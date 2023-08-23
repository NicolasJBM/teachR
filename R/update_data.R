#' @name update_data
#' @title Update course tests
#' @author Nicolas Mangin
#' @description Function listing tests and gathering tests definition parameters in a single database which is saved on disk.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @importFrom dplyr arrange
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr slice_head
#' @importFrom dplyr ungroup
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @importFrom tidyr unnest
#' @export

update_data <- function(course_paths){
  
  student <- NULL
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
  test <- NULL
  
  # Ratings
  ratings <- tibble::tibble(
    folder = base::list.files(
      course_paths$subfolders$ratings, recursive = FALSE, full.names = TRUE
    )
  ) |>
    dplyr::filter(!stringr::str_detect(folder, "archives$"))
  
  if (base::nrow(ratings) > 0){
    ratings <- ratings |>
      dplyr::mutate(
        ratings = purrr::map(folder, function(x){
          if (base::file.exists((x))) readr::read_csv(x, col_types = "ccn")
        })
      ) |>
      dplyr::select(-folder) |>
      tidyr::unnest(ratings) |>
      tidyr::separate(section, into = c("group","file"), sep = "-", remove = TRUE) |>
      tidyr::separate(file, into = c("code","language"), sep = "_", remove = FALSE) |>
      dplyr::mutate(language = stringr::str_remove_all(language, ".Rmd$"))
  } else {
    ratings <- tibble::tibble(
      timestamp = base::character(0),
      group = base::character(0),
      file = base::character(0),
      code = base::character(0),
      language = base::character(0),
      rate = base::numeric(0)
    )
  }
  base::save(ratings, file = course_paths$databases$ratings)
  
  # Comments
  comments <- tibble::tibble(
    folder = base::list.files(
      course_paths$subfolders$comments, recursive = FALSE, full.names = TRUE
    )
  ) |>
    dplyr::filter(!stringr::str_detect(folder, "archives$"))
  
  if (base::nrow(comments) > 0){
    comments <- comments |>
      dplyr::mutate(
        comments = purrr::map(folder, function(x){
          if (base::file.exists((x))) readr::read_csv(x, col_types = "ccc")
        })
      ) |>
      dplyr::select(-folder) |>
      tidyr::unnest(comments) |>
      base::unique() |>
      tidyr::separate(section, into = c("group","file"), sep = "-", remove = TRUE) |>
      tidyr::separate(file, into = c("code","language"), sep = "_", remove = FALSE) |>
      dplyr::mutate(language = stringr::str_remove_all(language, ".Rmd$"))
  } else {
    comments <- tibble::tibble(
      timestamp = base::character(0),
      group = base::character(0),
      file = base::character(0),
      code = base::character(0),
      language = base::character(0),
      comment = base::character(0)
    )
  }
  base::save(comments, file = course_paths$databases$comments)
  
  # Views
  views_data <- tibble::tibble(
    folder = base::list.files(
      course_paths$subfolders$views, recursive = FALSE, full.names = TRUE
    )
  ) |>
    dplyr::filter(!stringr::str_detect(folder, "archives$"))
  if (base::nrow(views_data) > 0){
    views_data <- views_data |>
      dplyr::mutate(
        views = purrr::map(folder, function(x){
          if (base::file.exists((x))) readr::read_csv(x, col_types = "cccnncnn")
        })
      ) |>
      dplyr::select(-folder) |>
      tidyr::unnest(views) 
    views_data <- views_data[,c(1,8,7,4,6,5)]
    base::names(views_data) <- c("tag_youtube","watchtime","views","viewers","duration","retention")
    views_data <- views_data |>
      dplyr::filter(tag_youtube != "Total")
    base::load(course_paths$databases$documents)
    views <- documents |>
      dplyr::select(tag_youtube, file, code, language) |>
      stats::na.omit() |>
      dplyr::left_join(views_data, by = "tag_youtube")
  } else {
    views <- tibble::tibble(
      tag_youtube = base::character(0),
      file = base::character(0),
      code = base::character(0),
      language = base::character(0),
      watchtime = base::numeric(0),
      views = base::numeric(0),
      viewers = base::numeric(0),
      duration = base::character(0),
      retention = base::numeric(0)
    )
  }
  base::save(views, file = course_paths$databases$views)
  
  # Students information
  students_info <- tibble::tibble(
    folder = base::list.files(
      course_paths$subfolders$students,
      recursive = FALSE, full.names = TRUE
    )
  ) |>
    dplyr::filter(!stringr::str_detect(folder, "archives$"))
  if (base::nrow(students_info) > 0){
    students_info <- students_info |>
      dplyr::mutate(
        students_info = purrr::map(folder, function(x){
          if (base::file.exists((x))) readr::read_csv(x, show_col_types = FALSE)
        })
      ) |>
      dplyr::select(-folder) |>
      tidyr::unnest(students_info)
  } else {
    students_info <- tibble::tibble(
      student = base::character(0),
      gender = base::character(0),
      age = base::numeric(0)
    )
  }
  
  # Tests, students, and results
  alltests <- tibble::tibble(
    test_folders = base::list.dirs(
      course_paths$subfolders$tests,
      recursive = FALSE, full.names = TRUE
    ),
    test = base::list.dirs(
      course_paths$subfolders$tests,
      recursive = FALSE, full.names = FALSE
    )
  )|>
    dplyr::filter(!stringr::str_detect(test_folders, "archives$|default$"))
  
  if (base::nrow(alltests) > 0){
    alltests <- alltests |>
      dplyr::mutate(
        tests = purrr::map(test_folders, function(x){
          base::load(base::paste0(x,"/test_parameters.RData"))
          return(test_parameters)
        }),
        students = purrr::map(test_folders, function(x, y){
          file <- base::paste0(x,"/6_students/student_list.csv")
          if (base::file.exists((file))) readr::read_csv(file, col_types = "ccccc") |>
            base::suppressWarnings()
        }),
        results = purrr::map(test_folders, function(x, y){
          file <- base::paste0(x,"/8_results/results.csv")
          if (base::file.exists((file))) readr::read_csv(file, col_types = "cnccncccccllnnnnn") |>
            base::suppressWarnings()
        }),
        grades = purrr::map(test_folders, function(x, y){
          file <- base::paste0(x,"/8_results/student_grades.csv")
          if (base::file.exists((file))) readr::read_csv(file, col_types = "cnnn") |>
            base::suppressWarnings()
        })
      )
    
    tests <- dplyr::select(alltests, tests) |>
      tidyr::unnest(tests)
    
    students <- dplyr::select(alltests, test, students) |>
      tidyr::unnest(students) |>
      dplyr::group_by(student) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup() |>
      dplyr::select(student, dplyr::everything()) |>
      dplyr::arrange(student) |>
      dplyr::left_join(students_info, by = "student")
    
    results <- dplyr::select(alltests, test, results) |>
      tidyr::unnest(results)
    
    grades <- dplyr::select(alltests, test, grades) |>
      tidyr::unnest(grades)
    
  } else {
    
    tests <- tibble::tibble(
      tree = base::as.character(NA),
      test = base::as.character(NA),
      test_format = base::as.character(NA),
      test_unit = base::as.character(NA),
      test_assessment = base::as.character(NA),
      test_documentation = base::as.character(NA),
      test_languages = base::as.character(NA),
      test_date = base::date(),
      test_duration = base::as.integer(NA),
      test_points = base::as.integer(NA),
      show_version = base::as.logical(NA),
      show_points = base::as.logical(NA),
      question = base::as.character(NA),
      section = base::as.character(NA),
      bloc = base::as.character(NA),
      altnbr = base::as.numeric(NA),
      points = base::as.numeric(NA),
      partial_credits = base::as.numeric(NA),
      penalty = base::as.numeric(NA),
      version = base::as.character(NA),
      seed = base::as.numeric(NA)
    ) |>
      stats::na.omit()
    
    students <- tibble::tibble(
      student = base::as.character(NA),
      test = base::as.character(NA),
      team = base::as.character(NA),
      firstname = base::as.character(NA),
      lastname = base::as.character(NA),
      email = base::as.character(NA),
      gender = base::as.character(NA),
      age = base::as.numeric(NA)
    ) |>
      stats::na.omit()
    
    results <- tibble::tibble(
      test = base::as.character(NA),
      student = base::as.character(NA),
      attempt = base::as.numeric(NA),
      question = base::as.character(NA),
      version = base::as.character(NA),
      number = base::as.numeric(NA),
      letter = base::as.character(NA),
      item = base::as.character(NA),
      document = base::as.character(NA),
      language = base::as.character(NA),
      scale = base::as.character(NA),
      partial_credits = base::as.logical(NA),
      penalty = base::as.logical(NA),
      points = base::as.numeric(NA),
      checked = base::as.numeric(NA),
      correct = base::as.numeric(NA),
      weight = base::as.numeric(NA),
      earned = base::as.numeric(NA)
    ) |>
      stats::na.omit()
    
    grades <- tibble::tibble(
      student = base::as.character(NA),
      attempt = base::as.numeric(NA),
      points = base::as.numeric(NA),
      grade = base::as.numeric(NA)
    ) |>
      stats::na.omit()
    
  }
  
  base::save(tests, file = course_paths$databases$tests)
  base::save(students, file = course_paths$databases$students)
  base::save(results, file = course_paths$databases$results)
  base::save(grades, file = course_paths$databases$grades)
}
