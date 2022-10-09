#' @name course_test_server
#' @title Load test data
#' @author Nicolas Mangin
#' @description Module facilitating the loading of relevant test data.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param tree Reactive. Function containing a list of documents as a classification tree compatible with jsTreeR
#' @return A list of course data.
#' @importFrom dplyr filter
#' @importFrom fs dir_copy
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny observe
#' @importFrom shiny reactive
#' @importFrom shiny req
#' @importFrom shiny updateSelectInput
#' @export


course_test_server <- function(id, course_data, course_paths, tree){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    test <- NULL
    
    tests <- shiny::reactive({
      shiny::req(!base::is.na(course_data()))
      shiny::req(!base::is.na(course_data()$tests))
      shiny::req(base::length(tree()) > 0)
      
      tests <- course_data()$tests
      
      test_selection <- base::unique(tests$test)
      
      if (base::length(tree()$course) > 1){
        course <- stringr::str_remove(tree()$course$tree[1], ".RData")
        course_tests <- stringr::str_detect(test_selection, base::paste0("^", course))
        test_selection <- c("", test_selection[course_tests])
      } else test_selection <- ""
      
      shiny::updateSelectInput(
        session,
        "selecttest",
        choices = test_selection
      )
      tests
    })
    
    shiny::observe({ tests() })
    
    selected_test <- shiny::reactive({
      shiny::req(input$selecttest != "")
      dplyr::filter(tests(), test == input$selecttest)
    })
    
    shiny::observe({ selected_test() })
    
    shiny::observeEvent(input$newtest, {
      shiny::req(!base::is.na(course_data()))
      shiny::req(!base::is.na(course_data()$tests))
      if (base::is.na(tree()$course[1])){
        shinyalert::shinyalert(
          "Select a tree first!",
          base::paste0('You need to select a tree to create a test.'),
          type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      } else {
        tmptestname <- stringr::str_replace(tree()$course[1], ".RData", "_newtest") 
        shiny::showModal(
          shiny::modalDialog(
            style = "background-color:#001F3F;color:#FFF;margin-top:300px;",
            shiny::textInput(
              ns("new_test_name"),
              "Name of the new test",
              value = tmptestname, width = "100%"
            ),
            footer = tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(
                ns("add_new_test"),
                "OK",
                icon = shiny::icon("check"),
                style = "background-color:#007777;color:#FFF;"
              )
            )
          )
        )
      }
    })
    
    shiny::observeEvent(input$add_new_test, {
      all_tests <- unique(course_data()$tests$test)
      shiny::removeModal()
      if (input$new_test_name %in% all_tests){
        shinyalert::shinyalert(
          "This name has already been assigned",
          'Please choose a different name.',
          type = "error", closeOnEsc = FALSE,
          closeOnClickOutside = TRUE
        )
      } else {
        test_template <- base::paste0(
          course_paths()$subfolders$tests, "/default"
        )
        new_test_folder <- base::paste0(
          course_paths()$subfolders$tests, "/", input$new_test_name
        )
        fs::dir_copy(test_template, new_test_folder)
        path_param <- base::paste0(new_test_folder, "/test_parameters.RData")
        base::load(path_param)
        test_parameters <- test_parameters[1,] |>
          dplyr::mutate(
            tree = modrval$tree,
            test = input$new_test_name,
            question = base::as.character(NA),
            section = base::as.character(NA),
            bloc = base::as.character(NA),
            altnbr = base::as.integer(NA),
            points = base::as.integer(NA),
            partial_credits = base::as.logical(NA),
            penalty = base::as.logical(NA),
            version = base::as.character(NA),
            seed = base::as.integer(NA)
          )
        base::save(test_parameters, file = path_param)
        modrval$tests <- c(modrval$tests, input$new_test_name)
        shinyalert::shinyalert(
          "Test created",
          base::paste0(
            'The test ',
            input$new_test_name,
            ' has been created. Update documents and reload the course to to select it.'
          ),
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      }
    })
    
    return(selected_test)
  })
}

