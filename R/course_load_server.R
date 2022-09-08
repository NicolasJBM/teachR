#' @name course_load_server
#' @title Load course data
#' @author Nicolas Mangin
#' @description Module facilitating the loading of all relevant course data.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return A list of course data.
#' @importFrom purrr map
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny observeEvent
#' @importFrom shiny reactiveValues
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom utils read.csv
#' @export


course_load_server <- function(id, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    document_models <- NULL
    document_parameters <- NULL
    document_types <- NULL
    documents <- NULL
    item_models <- NULL
    item_parameters <- NULL
    page_comments <- NULL
    page_ratings <- NULL
    propositions <- NULL
    students <- NULL
    tags <- NULL
    tests <- NULL
    translations <- NULL
    video_views <- NULL
    tree <- NULL
    jstree <- NULL
    langiso <- NULL
    courses <- NULL
    
    course_data <- shiny::reactiveValues()
      
    course_data$documents <- NA
    course_data$propositions <- NA
    course_data$translations <- NA
    course_data$document_types <- NA
    course_data$tags <- NA
    course_data$languages <- NA
    course_data$courses <- NA
    course_data$trees <- NA
    course_data$jstrees <- NA
    course_data$tests <- NA
    course_data$students <- NA
    course_data$page_ratings <- NA
    course_data$page_comments <- NA
    course_data$video_views <- NA
    course_data$document_parameters <- NA
    course_data$document_models <- NA
    course_data$item_parameters <- NA
    course_data$item_models <- NA
    
    
    shiny::observeEvent(input$loadcourse, {
      
      if (base::length(course_paths()) != 2){
        
        shinyalert::shinyalert(
          title = "Please select a course folder.",
          text = "You must first select a course folder to perform this action.",
          type = "error"
        )
        
      } else {
        
        shinybusy::show_modal_spinner(
          spin = "orbit",
          text = "Course loading..."
        )
        
        base::lapply(
          base::list.files(
            course_paths()$subfolders$functions,
            full.names = TRUE, recursive = TRUE
          ), base::source
        )
        
        if (base::file.exists(course_paths()$databases$documents)){
          base::load(course_paths()$databases$documents)
          course_data$documents <- documents
        } else course_data$documents <- NA
        
        if (base::file.exists(course_paths()$databases$propositions)){
          base::load(course_paths()$databases$propositions)
          course_data$propositions <- propositions
        } else course_data$propositions <- NA
        
        if (base::file.exists(course_paths()$databases$translations)){
          base::load(course_paths()$databases$translations)
          course_data$translations <- translations
        } else course_data$translations <- NA
        
        if (base::file.exists(course_paths()$databases$doctypes)){
          base::load(course_paths()$databases$doctypes)
          course_data$document_types <- document_types
        } else course_data$document_types <- NA
        
        if (base::file.exists(course_paths()$databases$tags)){
          base::load(course_paths()$databases$tags)
          course_data$tags <- tags
        } else course_data$tags <- NA
        
        course_data$languages <- tibble::tribble(
          ~langiso, ~language, ~link,
          "US",	"English (US)", "temporary/format/flags/us.svg",
          "GB",	"English (GB)", "temporary/format/flags/gb.svg",
          "FR",	"French", "temporary/format/flags/fr.svg",
          "ES",	"Spanish", "temporary/format/flags/es.svg",
          "IT",	"Italian", "temporary/format/flags/it.svg",
          "DE",	"German", "temporary/format/flags/de.svg",
          "NL",	"Dutch", "temporary/format/flags/nl.svg"
        )
        
        if (base::file.exists(course_paths()$databases$courses)){
          base::load(course_paths()$databases$courses)
          course_data$courses <- courses
        } else course_data$courses <- NA
        
        if (base::dir.exists(course_paths()$subfolders$jstrees)){
          tbltrees <- base::list.files(course_paths()$subfolders$trees)
          course_data$trees <- base::list()
          for (tbl in tbltrees) {
            base::load(base::paste0(course_paths()$subfolders$trees, "/", tbl))
            course_data$trees[[tbl]] <- tree
          }
        } else course_data$trees <- NA
        
        if (base::dir.exists(course_paths()$subfolders$jstrees)){
          jstrees <- base::list.files(course_paths()$subfolders$jstrees)
          course_data$jstrees <- base::list()
          for (jst in jstrees) {
            base::load(base::paste0(course_paths()$subfolders$jstrees, "/", jst))
            course_data$jstrees[[jst]] <- jstree
          }
        } else course_data$jstrees <- NA
        
        if (base::file.exists(course_paths()$databases$tests)){
          base::load(course_paths()$databases$tests)
          course_data$tests <- tests
        } else course_data$tests <- NA
        
        if (base::file.exists(course_paths()$databases$students)){
          base::load(course_paths()$databases$students)
          course_data$students <- students
        } else course_data$students <- NA
        
        if (base::file.exists(course_paths()$databases$page_ratings)){
          base::load(course_paths()$databases$page_ratings)
          course_data$page_ratings <- page_ratings
        } else course_data$page_ratings <- NA
        
        if (base::file.exists(course_paths()$databases$page_comments)){
          base::load(course_paths()$databases$page_comments)
          course_data$page_comments <- page_comments
        } else course_data$page_comments <- NA
        
        if (base::file.exists(course_paths()$databases$video_views)){
          base::load(course_paths()$databases$video_views)
          course_data$video_views <- video_views
        } else course_data$video_views <- NA
        
        if (base::file.exists(course_paths()$databases$document_parameters)){
          base::load(course_paths()$databases$document_parameters)
          course_data$document_parameters <- document_parameters
        } else course_data$document_parameters <- NA
        
        if (base::file.exists(course_paths()$databases$document_models)){
          base::load(course_paths()$databases$document_models)
          course_data$document_models <- document_models
        } else course_data$document_models <- NA
        
        if (base::file.exists(course_paths()$databases$item_parameters)){
          base::load(course_paths()$databases$item_parameters)
          course_data$item_parameters <- item_parameters
        } else course_data$item_parameters <- NA
        
        if (base::file.exists(course_paths()$databases$item_models)){
          base::load(course_paths()$databases$item_models)
          course_data$item_models <- item_models
        } else course_data$item_models <- NA
        
        
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
