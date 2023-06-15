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
#' @importFrom stats na.omit
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @importFrom tidyr unnest
#' @importFrom readr read_csv
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
  
  # Tests, students, and results
  alltests <- tibble::tibble(
    test_folders = base::list.dirs(
      course_paths$subfolders$tests, recursive = FALSE, full.names = TRUE
    )
  )|>
    dplyr::filter(!stringr::str_detect(test_folders, "archives$|default$"))
  
  if (base::nrow(alltests) > 0){
    
    alltests <- alltests|>
      dplyr::mutate(
        tests = purrr::map(test_folders, function(x){
          base::load(base::paste0(x,"/test_parameters.RData"))
          return(test_parameters)
        }),
        students = purrr::map(test_folders, function(x, y){
          file <- base::paste0(x,"/6_students/student_list.csv")
          if (base::file.exists((file))) readr::read_csv(file, col_types = "cccccc") |>
            base::suppressWarnings()
        }),
        results = purrr::map(test_folders, function(x, y){
          file <- base::paste0(x,"/8_results/results.csv")
          if (base::file.exists((file))) readr::read_csv(file, col_types = "ccnccnccccllnnnn") |>
            base::suppressWarnings()
        })
      )
    
    tests <- dplyr::select(alltests, tests) |>
      tidyr::unnest(tests)
    
    students_new <- dplyr::select(alltests, students) |>
      tidyr::unnest(students) |>
      dplyr::group_by(student) |>
      dplyr::sample_n(1) |>
      dplyr::ungroup() |>
      dplyr::select(student, dplyr::everything()) |>
      dplyr::arrange(student)
    if (base::file.exists(course_paths$databases$students)){
      base::load(course_paths$databases$students)
      students_new <- students_new |>
        dplyr::anti_join(students, by = c("student"))
      students <- students |>
        dplyr::bind_rows(students_new) |>
        base::unique()
    } else students <- students_new
    
    results <- dplyr::select(alltests, results) |>
      tidyr::unnest(results)
    
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
      test_duration = base::as.character(NA),
      test_points = base::as.character(NA),
      show_version = base::as.logical(0),
      show_points = base::as.logical(0),
      question = base::as.character(NA),
      section = base::as.character(NA),
      bloc = base::as.character(NA),
      altnbr = base::as.list.numeric_version(NA),
      points = base::as.list.numeric_version(NA),
      partial_credits = base::as.logical(0),
      penalty = base::as.logical(0),
      version = base::as.character(NA),
      seed = base::as.list.numeric_version(NA)
    ) |>
      stats::na.omit()
    
    students <- tibble::tibble(
      student = base::character(0),
      team = base::character(0),
      firstname = base::character(0),
      lastname = base::character(0),
      email = base::character(0),
    )
    
    results <- tibble::tibble(
      student = base::character(0),
      test = base::character(0),
      attempt = base::integer(0),
      question = base::character(0),
      version = base::character(0),
      number = base::integer(0),
      letter = base::character(0),
      item = base::character(0),
      language = base::character(0),
      scale = base::character(0),
      partial_credits = base::logical(0),
      penalty = base::logical(0),
      points = base::numeric(0),
      checked = base::numeric(0),
      weight = base::numeric(0),
      earned = base::numeric(0)
    )
  }
  
  base::save(tests, file = course_paths$databases$tests)
  base::save(students, file = course_paths$databases$students)
  base::save(results, file = course_paths$databases$results)
}
