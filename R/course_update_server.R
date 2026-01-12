#' @name course_update_server
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


course_update_server <- function(id, course_paths){
  ns <- shiny::NS(id)
  
  propositions <- NULL
  translations <- NULL
  
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$updatecourse, {
      
      if (base::length(course_paths()) != 2){
        
        shinyalert::shinyalert(
          title = "Please select a course folder.",
          text = "You must first select a course folder to perform this action.",
          type = "error"
        )
        
      } else {
        
        # Read documents and update tags and tree accordingly.
        
        shinybusy::show_modal_progress_line(value = 0/12, text = "Updating documents")
        
        teachR::update_documents(course_paths())
        
        shinybusy::update_modal_progress(value = 1/12, text = "Updating tags")
        
        teachR::update_tags(course_paths())
        
        shinybusy::update_modal_progress(value = 2/12, text = "Updating trees")
        
        teachR::update_trees(course_paths())
        
        
        
        
        
        shinybusy::update_modal_progress(value = 5/12, text = "Updating tests")
        
        teachR::update_tests(course_paths())
        
        shinybusy::update_modal_progress(value = 10/12, text = "Updating solutions")
        
        teachR::update_solutions(course_paths())
        
        
        
        
        
        shinybusy::update_modal_progress(value = 3/12, text = "Updating paths")
        
        teachR::update_paths(course_paths())
        
        
        
        
        
        
        
        
        
        shinybusy::update_modal_progress(value = 4/12, text = "Updating students")
        
        teachR::update_students(course_paths())
        
        
        shinybusy::update_modal_progress(value = 6/12, text = "Updating logs")
        
        teachR::update_logs(course_paths())
        
        shinybusy::update_modal_progress(value = 7/12, text = "Updating views")
        
        teachR::update_views(course_paths())
        
        shinybusy::update_modal_progress(value = 8/12, text = "Updating ratings")
        
        teachR::update_ratings(course_paths())
        
        shinybusy::update_modal_progress(value = 9/12, text = "Updating comments")
        
        teachR::update_comments(course_paths())
        
        shinybusy::update_modal_progress(value = 11/12, text = "Updating answers")
        
        teachR::update_answers(course_paths())
      }
      
    })
    
    
    
    shiny::observeEvent(input$updatepackage, {
      
      if (base::length(course_paths()) != 2){
        
        shinyalert::shinyalert(
          title = "Please select a course folder.",
          text = "You must first select a course folder to perform this action.",
          type = "error"
        )
        
      } else {
        
        shinybusy::show_modal_progress_line(value = 0/6, text = "Updating the package")
        
        # Create or empty the function folder for the course package
        dfRfolder <- base::paste0(course_paths()$subfolders$package, "/R")
        if (!base::dir.exists(dfRfolder)) base::dir.create(dfRfolder)
        base::unlink(base::list.files(dfRfolder, full.names = TRUE))
        
        shinybusy::update_modal_progress(value = 1/6, text = "Add functions to the package")
        
        # copy functions
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
        
        shinybusy::update_modal_progress(value = 2/6, text = "Updating package main data")
        
        # Create or empty the data folder for the course package and copy functions
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
        
        shinybusy::update_modal_progress(value = 3/6, text = "Updating package secondary data")
        
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
        
        shinybusy::update_modal_progress(value = 4/6, text = "Save the environment for editing documents")
        
        base::save.image(file=base::paste0(course_paths()$subfolders$edit, '/data/environment.RData'))
        
        
        shinybusy::update_modal_progress(value = 5/6, text = "Import questions")
        
        original <- base::list.files(course_paths()$subfolders$original, full.names = FALSE, pattern = "^Q")
        original <- base::paste0(course_paths()$subfolders$original, "/", original)
        translated <- base::list.files(course_paths()$subfolders$translated, full.names = FALSE, pattern = "^Q")
        translated <- base::paste0(course_paths()$subfolders$translated, "/", translated)
        destination <- base::paste0(course_paths()$subfolders$package,"/inst/exercises")
        
        base::file.copy(from = original, to = destination, overwrite = TRUE)
        base::file.copy(from = translated, to = destination, overwrite = TRUE)
        
        
        shinybusy::update_modal_progress(value = 6/6, text = "Package updated")
        
        shinybusy::remove_modal_spinner()
        
        shinyalert::shinyalert(
          title = "Package updated!",
          text = "Reinstall the package to use it.",
          type = "success"
        )
      }
    })
    
  })
}

