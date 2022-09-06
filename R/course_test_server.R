#' @name course_test_server
#' @title Load test data
#' @author Nicolas Mangin
#' @description Module facilitating the loading of relevant test data.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param tree Reactive. Function containing a list of documents as a classification tree compatible with jsTreeR
#' @return A list of course data.
#' @importFrom dplyr filter
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny observe
#' @importFrom shiny reactive
#' @importFrom shiny req
#' @importFrom shiny updateSelectInput
#' @export


course_test_server <- function(id, course_data, tree){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    test <- NULL
    
    tests <- shiny::reactive({
      shiny::req(!base::is.na(course_data()))
      shiny::req(!base::is.na(course_data()$tests))
      
      tests <- course_data()$tests
      
      test_selection <- base::unique(tests$test)
      
      if (base::length(tree()$course) > 1){
        course <- stringr::str_remove(tree()$course$tree[1], ".RData")
        course_tests <- stringr::str_detect(test_selection, base::paste0("^", course))
        test_selection <- test_selection[course_tests]
      }
      
      shiny::updateSelectInput(
        session,
        "selecttest",
        choices = c("", test_selection)
      )
      tests
    })
    
    shiny::observe({ tests() })
    
    selected_test <- shiny::reactive({
      shiny::req(input$selecttest != "")
      dplyr::filter(tests(), test == input$selecttest)
    })
    
    shiny::observe({ selected_test() })
    
    return(selected_test)
  })
}

