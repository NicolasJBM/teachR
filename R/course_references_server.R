#' @name course_references_server
#' @title Load course references
#' @author Nicolas Mangin
#' @description Module facilitating the loading of all relevant references for the course.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Tibble with references.
#' @importFrom bibliogR make_bib_file
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny req
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom utils installed.packages
#' @export


course_references_server <- function(id, course_paths, references){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$updateref, {
      
      if (base::length(course_paths()) == 1){
        
        shinyalert::shinyalert(
          title = "Missing selections!",
          text = "You need to select a course folder to update course references.",
          type = "error"
        )
        
      } else {
        
        if ("bibliogR" %in% base::as.data.frame(utils::installed.packages())$Package){
          
          shinybusy::show_modal_spinner(
            spin = "orbit",
            text = "Updating references..."
          )
          
          bibliogR::make_bib_file(
            source_folder = c(
              course_paths()$subfolders$original,
              course_paths()$subfolders$translated
            ),
            references = references(),
            destination_folder = base::paste0(
              course_paths()$subfolders$edit, "/data"
            ),
            file_name = "references.bib"
          )
          
          shinybusy::remove_modal_spinner()
          
          shinyalert::shinyalert(
            title = "References updated!",
            text = "Your references are now up-to-date.",
            type = "success"
          )
          
        } else {
          
          shinyalert::shinyalert(
            title = "Missing package bibliogR!",
            text = "To work with references, you need to install the package bibliogR first.",
            type = "error"
          )
          
        }
        
      }
      
    })
  })
}

