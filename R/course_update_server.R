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
        
        
        
        
        
        shinybusy::update_modal_progress(value = 5/12, text = "Updating tests")
        
        teachR::update_tests(course_paths())
        
        shinybusy::update_modal_progress(value = 10/12, text = "Updating solutions")
        
        teachR::update_solutions(course_paths())
        
        
        
        
        
        
        
        
        
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
        
      }
    })
    
  })
}

