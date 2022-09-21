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


course_references_server <- function(id, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    references <- shiny::reactive({
      
      shiny::req(!base::is.null(input$referencefile))
      
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = "References loading..."
      )
      
      base::load(input$referencefile$datapath) |>
        base::suppressWarnings()
      
      shinybusy::remove_modal_spinner()
      
      shinyalert::shinyalert(
        title = "References loaded!",
        text = "All references are now loaded.",
        type = "success"
      )
      
      references
    })
    
    shiny::observe({ references() })
    
    shiny::observeEvent(input$updateref, {
      
      if (base::length(course_paths()) == 1 | base::is.null(references())){
        
        shinyalert::shinyalert(
          title = "Missing selections!",
          text = "You need to select a course folder and references to update course references.",
          type = "error"
        )
        
      } else {
        
        if ("bibliogR" %in% base::as.data.frame(utils::installed.packages())$Package){
          
          bibliogR::make_bib_file(
            source_folder = c(
              course_paths()$subfolders$original,
              course_paths()$subfolders$translated
            ),
            references = references(),
            destination_folder = base::paste0(
              course_paths()$subfolders$course, "/temporary/data"
            ),
            file_name = "references.bib"
          )
          
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
    
    return(references)
  })
}

