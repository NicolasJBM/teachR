#' @name statistics_update_server
#' @title Update documents and data in a course.
#' @author Nicolas Mangin
#' @description Module facilitating the update of underlying course documents, trees, data, and statistics.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @importFrom shiny NS
#' @importFrom shiny moduleServer
#' @importFrom shiny observeEvent
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @export


statistics_update_server <- function(id, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$updatestatistics,{
      if (base::length(course_paths()) != 2){
        
        shinyalert::shinyalert(
          title = "Please select a course folder.",
          text = "You must first select a course folder to perform this action.",
          type = "error"
        )
        
      } else {
        
        shinybusy::show_modal_progress_line(value = 0/3, text = "Updating data")
        
        teachR::update_data(course_paths())
        
        shinybusy::update_modal_progress(value = 1/3, text = "Updating statistics")
        
        teachR::update_statistics(course_paths())
        
        shinybusy::update_modal_progress(value = 3/3, text = "Statistics updated")
        
        shinybusy::remove_modal_spinner()
        
        shinyalert::shinyalert(
          title = "Data and statistics updated!",
          text = "Data and statistics are now up-to-date. Load the course to apply changes.",
          type = "success"
        )
      }
    })
    
  })
}

