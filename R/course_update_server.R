#' @name course_update_server
#' @title Update documents and data
#' @author Nicolas Mangin
#' @description Module facilitating the update of underlying course data.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Nothing. All changes ore on disk
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny observeEvent
#' @importFrom shinyalert shinyalert
#' @export


course_update_server <- function(id, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    ############################################################################
    # Documents
    shiny::observeEvent(input$updatedocuments, {
      
      if (base::length(course_paths()) != 2){
        
        shinyalert::shinyalert(
          title = "Please select a course folder.",
          text = "You must first select a course folder to perform this action.",
          type = "error"
          
        )
      } else {
        
        shinybusy::show_modal_spinner(
          spin = "orbit",
          text = "Please wait while the application updates documents and tags..."
        )
        
        teachR::update_documents(course_paths())
        teachR::update_tags(course_paths())
        
        shinybusy::remove_modal_spinner()
        
        shinyalert::shinyalert(
          title = "Documents and tags updated!",
          text = "Documents and tags are now up-to-date. Load the course to apply changes.",
          type = "success"
        )
        
      }
      
    })
    
    
    
    ############################################################################
    # Trees
    shiny::observeEvent(input$updatetrees,{
      
      if (base::length(course_paths()) != 2){
        
        shinyalert::shinyalert(
          title = "Please select a course folder.",
          text = "You must first select a course folder to perform this action.",
          type = "error"
        )
        
      } else {
        
        shinybusy::show_modal_spinner(
          spin = "orbit",
          text = "Please wait while the application updates trees. This takes a very long time..."
        )
        
        teachR::update_trees(course_paths())
        
        shinybusy::remove_modal_spinner()
        
        shinyalert::shinyalert(
          title = "Trees updated!",
          text = "Trees are now up-to-date. Load the course to apply changes.",
          type = "success"
        )
        
      }
      
    })
    
    
    
    ############################################################################
    # Statistics
    shiny::observeEvent(input$updatestatistics,{
      if (base::length(course_paths()) != 2){
        
        shinyalert::shinyalert(
          title = "Please select a course folder.",
          text = "You must first select a course folder to perform this action.",
          type = "error"
        )
        
      } else {
        
        shinybusy::show_modal_spinner(
          spin = "orbit",
          text = "Please wait while the application updates data and statistics. This takes a very long time..."
        )
        
        teachR::update_data(course_paths())
        teachR::update_statistics(course_paths())
        
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

