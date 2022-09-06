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
        
        teachR::update_documents(course_paths())
        
      }
      
    })
    
    ############################################################################
    # Tags
    shiny::observeEvent(input$updatetags, {
      
      if (base::length(course_paths()) != 2){
        
        shinyalert::shinyalert(
          title = "Please select a course folder.",
          text = "You must first select a course folder to perform this action.",
          type = "error"
        )
        
      } else {
        
        teachR::update_tags(course_paths())
        
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
        
        teachR::update_trees(course_paths())
        
      }
      
    })
    
    ############################################################################
    # Data
    shiny::observeEvent(input$updatedata,{
      
      if (base::length(course_paths()) != 2){
        
        shinyalert::shinyalert(
          title = "Please select a course folder.",
          text = "You must first select a course folder to perform this action.",
          type = "error"
        )
        
      } else {
        
        teachR::update_data(course_paths())
        
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
        
        teachR::update_statistics(course_paths())
        
      }
    })
    
  })
}

