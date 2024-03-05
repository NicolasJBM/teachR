#' @name course_load_server
#' @title Load course data
#' @author Nicolas Mangin
#' @description Module facilitating the loading of all relevant course data.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return A list of course data.
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
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
    
    course_data <- shiny::reactiveValues()
      
    course_data$documents <- NA
    course_data$document_types <- NA
    course_data$tags <- NA
    course_data$languages <- NA
    course_data$courses <- NA
    course_data$tbltrees <- NA
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
        
        shinybusy::show_modal_progress_line(value = 0/4, text = "Updating documents")
        
        teachR::update_documents(course_paths())
        
        shinybusy::update_modal_progress(value = 1/4, text = "Updating tags")
        
        teachR::update_tags(course_paths())
        
        shinybusy::update_modal_progress(value = 2/4, text = "Updating trees")
        
        teachR::update_trees(course_paths())
        
        shinybusy::update_modal_progress(value = 3/4, text = "Loading course")
        
        dfRfolder <- base::paste0(course_paths()$subfolders$package, "/R")
        if (!base::dir.exists(dfRfolder)) base::dir.create(dfRfolder)
        base::unlink(base::list.files(dfRfolder, full.names = TRUE))
        
        base::lapply(
          base::list.files(
            course_paths()$subfolders$functions,
            full.names = TRUE, recursive = TRUE
          ), function(x){
            if (stringr::str_detect(x, "\\.R$")) {
              base::source(x)
              base::file.copy(
                from = x,
                to = stringr::str_replace(
                  x,
                  course_paths()$subfolders$functions,
                  dfRfolder
                ),
                overwrite = TRUE
              )
            }
          }
        )
        
        dfdatafolder <- base::paste0(course_paths()$subfolders$package, "/data")
        if (!base::dir.exists(dfdatafolder)) base::dir.create(dfdatafolder)
        base::unlink(base::list.files(dfdatafolder, full.names = TRUE))
        
        databases <- base::list.files(course_paths()$subfolders$databases, full.names = FALSE)
        databases <- databases[stringr::str_detect(databases, "csv$|xlsx$")]
        
        base::load(course_paths()$databases$propositions)
        scholR::document_data(
          x = propositions,
          datname = "propositions",
          path = dfRfolder
        )
        base::save(propositions, file=base::paste0(dfdatafolder, '/propositions.RData'))
        tools::resaveRdaFiles(base::paste0(dfdatafolder, '/propositions.RData'))
        
        base::load(course_paths()$databases$translations)
        scholR::document_data(
          x = translations,
          datname = "translations",
          path = dfRfolder
        )
        base::save(translations, file=base::paste0(dfdatafolder, '/translations.RData'))
        tools::resaveRdaFiles(base::paste0(dfdatafolder, '/translations.RData'))
        
        if (base::length(databases) > 0){
          for (d in databases){
            path <- base::paste0(course_paths()$subfolders$databases, "/", d)
            if (stringr::str_detect(path, "csv$")){
              dcontent <- readr::read_csv(path, show_col_types = FALSE)
              dname <- stringr::str_remove(d, ".csv$")
            } else {
              dcontent <- readxl::read_excel(path)
              dname <- stringr::str_remove(d, ".xlsx$")
            }
            base::assign(x = dname, value = dcontent, envir = .GlobalEnv)
            scholR::document_data(
              x = dcontent,
              datname = dname,
              path = dfRfolder
            )
            
            base::save(list = dname, file=base::paste0(dfdatafolder, '/', dname, '.RData'))
            tools::resaveRdaFiles(base::paste0(dfdatafolder, '/', dname, '.RData'))
            
          }
          base::rm(databases, d, dcontent, dname)
        }
        
        base::save.image(file=base::paste0(course_paths()$subfolders$edit, '/data/environment.RData'))
        
        if (base::file.exists(course_paths()$databases$documents)){
          base::load(course_paths()$databases$documents)
          course_data$documents <- documents
        } else course_data$documents <- NA
        
        if (base::file.exists(course_paths()$databases$doctypes)){
          base::load(course_paths()$databases$doctypes)
          course_data$document_types <- document_types
        } else course_data$document_types <- NA
        
        if (base::file.exists(course_paths()$databases$tags)){
          base::load(course_paths()$databases$tags)
          course_data$tags <- tags
        } else course_data$tags <- NA
        
        course_data$languages <- readr::read_csv(
          base::paste0(course_paths()$subfolders$course, "languages.csv"),
          show_col_types = FALSE
        )
        
        if (base::file.exists(course_paths()$databases$courses)){
          base::load(course_paths()$databases$courses)
          course_data$courses <- courses
        } else course_data$courses <- NA
        
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
        
        shinybusy::update_modal_progress(value = 4/4, text = "Course loaded")
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

