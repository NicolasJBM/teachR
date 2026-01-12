#' @name course_load_server
#' @title Load course data
#' @author Nicolas Mangin
#' @description Module facilitating the loading of all relevant course data.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return A list of course data.
#' @importFrom readr read_csv
#' @importFrom scholR document_data
#' @importFrom shiny NS
#' @importFrom shiny moduleServer
#' @importFrom shiny observeEvent
#' @importFrom shiny reactiveValues
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_progress_line
#' @importFrom shinybusy update_modal_progress
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace
#' @importFrom tools resaveRdaFiles
#' @importFrom cryptography autokey
#' @export


course_load_server <- function(id, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    # Bind variables
    document_models <- NULL
    document_parameters <- NULL
    document_types <- NULL
    documents <- NULL
    item_models <- NULL
    item_parameters <- NULL
    comments <- NULL
    ratings <- NULL
    students <- NULL
    tags <- NULL
    tests <- NULL
    video_views <- NULL
    tbltree <- NULL
    jstree <- NULL
    langiso <- NULL
    courses <- NULL
    propositions <- NULL
    translations <- NULL
    activities <- NULL
    actlabels <- NULL
    answers <- NULL
    connections <- NULL
    grades <- NULL
    intakes <- NULL
    logs <- NULL
    outcomes <- NULL
    outlabels <- NULL
    results <- NULL
    scores <- NULL
    views <- NULL
    paths <- NULL
    solutions <- NULL
    email <- NULL
    firstname <- NULL
    lastname <- NULL
    studenturl <- NULL
    
    # Create course content list
    
    course_data <- shiny::reactiveValues()
    
    # Prepare for course organization and structure
    course_data$languages <- NA
    course_data$document_types <- NA
    course_data$tags <- NA
    course_data$intakes <- NA
    course_data$tbltrees <- NA
    course_data$jstrees <- NA
    course_data$outcomes <- NA
    course_data$connections <- NA
    course_data$outlabels <- NA
    course_data$activities <- NA
    course_data$actlabels <- NA
    course_data$attributes <- NA
    course_data$students <- NA
    course_data$documents <- NA
    course_data$tests <- NA
    
    # Prepare for course analytics and statistics
    course_data$logs <- NA
    course_data$views <- NA
    course_data$ratings <- NA
    course_data$comments <- NA
    course_data$solutions <- NA
    course_data$answers <- NA
    course_data$results <- NA
    course_data$scores <- NA
    course_data$grades <- NA
    course_data$document_parameters <- NA
    course_data$item_parameters <- NA
    
    shiny::observeEvent(input$loadcourse, {
      
      if (base::length(course_paths()) != 2){
        
        shinyalert::shinyalert(
          title = "Please select a course folder.",
          text = "You must first select a course folder to perform this action.",
          type = "error"
        )
        
      } else {
        
        course_data$languages <- readr::read_csv(
          base::paste0(course_paths()$subfolders$course, "languages.csv"),
          show_col_types = FALSE
        )
        
        if (base::file.exists(course_paths()$databases$doctypes)){
          base::load(course_paths()$databases$doctypes)
          course_data$document_types <- document_types
        } else course_data$document_types <- NA
        
        if (base::file.exists(course_paths()$databases$tags)){
          base::load(course_paths()$databases$tags)
          course_data$tags <- tags
        } else course_data$tags <- NA
        
        if (base::file.exists(course_paths()$databases$intakes)){
          base::load(course_paths()$databases$intakes)
          course_data$intakes <- intakes
        } else course_data$intakes <- NA
        
        if (base::dir.exists(course_paths()$subfolders$tbltrees)){
          tbltrees <- base::list.files(course_paths()$subfolders$tbltrees)
          course_data$tbltrees <- base::list()
          for (tbl in tbltrees) {
            base::load(base::paste0(course_paths()$subfolders$tbltrees, "/", tbl))
            course_data$tbltrees[[tbl]] <- tbltree
          }
        } else course_data$tbltrees <- NA
        
        if (base::dir.exists(course_paths()$subfolders$jstrees)){
          jstrees <- base::list.files(course_paths()$subfolders$jstrees)
          course_data$jstrees <- base::list()
          for (jst in jstrees) {
            base::load(base::paste0(course_paths()$subfolders$jstrees, "/", jst))
            course_data$jstrees[[jst]] <- jstree
          }
        } else course_data$jstrees <- NA
        
        if (base::file.exists(course_paths()$databases$paths)){
          base::load(course_paths()$databases$paths)
          course_data$outcomes <- paths$outcomes
          course_data$connections <- paths$connections
          course_data$outlabels <- paths$outlabels
          course_data$activities <- paths$activities
          course_data$actlabels <- paths$actlabels
          course_data$attributes <- paths$attributes
        } else {
          course_data$outcomes <- NA
          course_data$connections <- NA
          course_data$outlabels <- NA
          course_data$activities <- NA
          course_data$actlabels <- NA
          course_data$attributes <- NA
        }
        
        if (base::file.exists(course_paths()$databases$students)){
          base::load(course_paths()$databases$students)
          if (base::nchar(input$encryptkey) > 4){
            key <- input$encryptkey
            students <- students |>
              teachR::decrypt_variable("firstname", key) |>
              teachR::decrypt_variable("lastname", key) |>
              teachR::decrypt_variable("email", key) |>
              teachR::decrypt_variable("studenturl", key)
          } else {
            students <- students
          }
          course_data$students <- students
        } else {
          course_data$students <- NA
        }
        
        if (base::file.exists(course_paths()$databases$documents)){
          base::load(course_paths()$databases$documents)
          course_data$documents <- documents
        } else course_data$documents <- NA
        
        if (base::file.exists(course_paths()$databases$tests)){
          base::load(course_paths()$databases$tests)
          course_data$tests <- tests
        } else course_data$tests <- NA
        
        if (base::file.exists(course_paths()$databases$logs)){
          base::load(course_paths()$databases$logs)
          course_data$logs <- logs
        } else course_data$logs <- NA
        
        if (base::file.exists(course_paths()$databases$views)){
          base::load(course_paths()$databases$views)
          course_data$views <- views
        } else course_data$views <- NA
        
        if (base::file.exists(course_paths()$databases$ratings)){
          base::load(course_paths()$databases$ratings)
          course_data$ratings <- ratings
        } else course_data$ratings <- NA
        
        if (base::file.exists(course_paths()$databases$comments)){
          base::load(course_paths()$databases$comments)
          course_data$comments <- comments
        } else course_data$comments <- NA
        
        if (base::file.exists(course_paths()$databases$solutions)){
          base::load(course_paths()$databases$solutions)
          course_data$solutions <- solutions
        } else course_data$solutions <- NA
        
        if (base::file.exists(course_paths()$databases$answers)){
          base::load(course_paths()$databases$answers)
          course_data$answers <- answers
        } else course_data$answers <- NA
        
        if (base::file.exists(course_paths()$databases$results)){
          base::load(course_paths()$databases$results)
          course_data$results <- results
        } else course_data$results <- NA
        
        if (base::file.exists(course_paths()$databases$scores)){
          base::load(course_paths()$databases$scores)
          course_data$scores <- scores
        } else course_data$scores <- NA
        
        if (base::file.exists(course_paths()$databases$grades)){
          base::load(course_paths()$databases$grades)
          course_data$grades <- grades
        } else course_data$grades <- NA
        
        if (base::file.exists(course_paths()$databases$document_parameters)){
          base::load(course_paths()$databases$document_parameters)
          course_data$document_parameters <- document_parameters
        } else course_data$document_parameters <- NA
        
        if (base::file.exists(course_paths()$databases$item_parameters)){
          base::load(course_paths()$databases$item_parameters)
          course_data$item_parameters <- item_parameters
        } else course_data$item_parameters <- NA
        
        shinybusy::update_modal_progress(value = 13/13, text = "Course updated")
        
        shinybusy::remove_modal_spinner()
        
        shinyalert::shinyalert(
          title = "Course loaded!",
          text = "All course data are now loaded.",
          type = "success"
        )
      }
      
    })
    
    return(course_data)
  })
}

