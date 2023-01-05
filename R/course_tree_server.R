#' @name course_tree_server
#' @title Load tree data
#' @author Nicolas Mangin
#' @description Module facilitating the loading of a js classification tree.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return A list which can be treated as a sTreeR object.
#' @importFrom classR trees_structure_textbook
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom shiny actionButton
#' @importFrom shiny icon
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny numericInput
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny removeModal
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny showModal
#' @importFrom shiny textInput
#' @importFrom shiny selectizeInput
#' @importFrom shiny updateSelectInput
#' @importFrom shinyalert shinyalert
#' @importFrom tibble tibble
#' @export


course_tree_server <- function(id, course_data, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    tree <- NULL
    
    shiny::observe({
      shiny::req(!base::is.na(course_data()$courses))
      shiny::updateSelectInput(
        session, "selecttree",
        choices = c("", course_data()$courses$tree)
      )
    })
    
    selected_tree <- shiny::reactive({
      shiny::req(!base::is.null(input$selecttree))
      
      if (input$selecttree != ""){
        
        if (input$selecttree %in% base::names(course_data()$trees)){
          
          course <- dplyr::filter(course_data()$courses, tree == input$selecttree)
          tbltree <- course_data()$trees[[input$selecttree]]
          jstree <- course_data()$jstrees[[input$selecttree]]
          textbook <- classR::trees_structure_textbook(
            tbltree, course$tree[1], course$website[1]
          )
          
          base::list(
            course = course,
            tbltree = tbltree,
            jstree = jstree,
            textbook = textbook
          )
          
        } else {
          
          base::list(
            course = NA,
            tbltree = NA,
            jstree = NA,
            textbook = NA
          )
          
          shinyalert::shinyalert(
            "This tree does not exist!",
            "Reload the course to update the list of trees.",
            type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
          )
          
        }
        
      } else {
        
        base::list(
          course = NA,
          tbltree = NA,
          jstree = NA,
          textbook = NA
        )
        
      }
    })
    
    shiny::observe({ selected_tree() })
    
    shiny::observeEvent(input$newtree, {
      if (base::length(course_data()$courses) == 1){
        shinyalert::shinyalert(
          "Load a course first!",
          "You need to load a course to create a new tree.",
          type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      } else {
        courses <- course_data()$courses
        last_course <- courses[1,]
        year <- base::substr(base::Sys.Date(),1,4)
        shiny::showModal(
          shiny::modalDialog(
            style = "background-color:#001F3F;color:#FFF;margin-top:300px;",
            shiny::textInput(
              ns("newtreename"), "Save tree as:",
              value = "new_tree.RData", width = "100%"
            ),
            shiny::selectizeInput(
              ns("newtreebasis"), "Base new tree on:",
              choices = base::unique(courses$tree),
              selected = "unclassified.RData",
              width = "100%", options = base::list(create = FALSE)
            ),
            shiny::selectizeInput(
              ns("newtreecourse"), "Course:",
              choices = base::unique(courses$course),
              selected = last_course$course[1],
              width = "100%", options = base::list(create = TRUE)
            ),
            shiny::textInput(
              ns("newtreeauthors"), "Authors:",
              value = last_course$authors[1], width = "100%"
            ),
            shiny::selectizeInput(
              ns("newtreeinstitution"), "Institution:",
              choices = base::unique(courses$institution),
              selected = last_course$institution[1],
              width = "100%", options = base::list(create = TRUE)
            ),
            shiny::selectizeInput(
              ns("newtreeprogram"), "Program:",
              choices = base::unique(courses$program),
              selected = last_course$program[1],
              width = "100%", options = base::list(create = TRUE)
            ),
            shiny::selectizeInput(
              ns("newtreeprogramlevel"), "Program level:",
              choices = base::unique(courses$program_level),
              selected = last_course$program_level[1],
              width = "100%", options = base::list(create = TRUE)
            ),
            shiny::textInput(
              ns("newtreegroup"), "Group:",
              value = "new_group_name", width = "100%"
            ),
            shiny::numericInput(
              ns("newtreeyear"), "Year:", value = year, width = "100%"
            ),
            shiny::textInput(
              ns("newtreewebsite"), "Website:",
              value = last_course$website[1], width = "100%"
            ),
            footer = tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(
                ns("makenewtree"), "OK",
                icon = shiny::icon("check"),
                style = "background-color:#007777;color:#FFF;"
              )
            )
          )
        )
      }
    })
    
    shiny::observeEvent(input$makenewtree, {
      shiny::removeModal()
      new <- tibble::tibble(
        tree = base::as.character(input$newtreename),
        course = base::as.character(input$newtreecourse),
        authors = base::as.character(input$newtreeauthors),
        institution = base::as.character(input$newtreeinstitution),
        program = base::as.character(input$newtreeprogram),
        program_level = base::as.character(input$newtreeprogramlevel),
        group = base::as.character(input$newtreegroup),
        year =  base::as.character(input$newtreeyear),
        website = base::as.character(input$newtreewebsite)
      )
      courses <- dplyr::bind_rows(new, course_data()$courses)
      base::save(courses, file = course_paths()$databases$courses)
      base::file.copy(
        from = base::paste0(course_paths()$subfolders$trees, "/", input$newtreebasis),
        to = base::paste0(course_paths()$subfolders$trees, "/", input$newtreename)
      )
      base::file.copy(
        from = base::paste0(course_paths()$subfolders$jstrees, "/", input$newtreebasis),
        to = base::paste0(course_paths()$subfolders$jstrees, "/", input$newtreename)
      )
      shinyalert::shinyalert(
        "Classification tree created!",
        "The new tree has been created. Reload the course to enact changes.",
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
    return(selected_tree)
  })
}

