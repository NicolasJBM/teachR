#' @name course_intake_server
#' @title Load intake data
#' @author Nicolas Mangin
#' @description Module facilitating the loading of an intake (with associated trees, paths, and tests).
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param selected_intake Reactive. Name of the selected intake.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return A list containing the intake as a table and a json object.
#' @importFrom classR trees_structure_textbook
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny icon
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny numericInput
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny removeModal
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectizeInput
#' @importFrom shiny showModal
#' @importFrom shiny textInput
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets searchInput
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @export


course_intake_server <- function(id, selected_intake, course_data, course_paths){
  
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    edited_intake <- shiny::reactive({
      shiny::req(!base::is.null(selected_intake()) & !base::is.na(selected_intake()))
      shiny::req(!base::is.null(course_data()$intakes) & !base::is.na(course_data()$intakes))
      
      intake <- dplyr::filter(course_data()$intakes, intake == selected_intake())
      students <- course_data()$students |>
        dplyr::filter(intake == selected_intake()) |>
        dplyr::select(-intake)
      
      base::list(
        intake = intake,
        students = students
      )
    })
    
    output$editintake <- shiny::renderUI({
      shiny::req(base::length(edited_intake()) == 2)
      
      allpaths <- base::unique(course_data()$outcomes$path)
      alltrees <- stringr::str_remove_all(base::names(course_data()$tbltrees), ".RData")
      
      base::list(
        shiny::h4(edited_intake()$intake$intake[1]),
        shiny::textInput(
          ns("teachers"), "Teachers:",
          value = edited_intake()$intake$teachers[1], width = "100%"
        ),
        shiny::textInput(
          ns("institution"), "Institution:",
          value = edited_intake()$intake$institution[1], width = "100%"
        ),
        shiny::textInput(
          ns("program"), "Program:",
          value = edited_intake()$intake$program[1], width = "100%"
        ),
        shiny::textInput(
          ns("programlevel"), "Program level:",
          value = edited_intake()$intake$program_level[1], width = "100%"
        ),
        shiny::numericInput(
          ns("year"), "Year:",
          value = edited_intake()$intake$year[1], width = "100%"
        ),
        shiny::selectInput(
          ns("path"), "Path:", choices = allpaths,
          selected = edited_intake()$intake$path[1], width = "100%"
        ),
        shiny::selectInput(
          ns("tree"), "Tree:", choices = alltrees,
          selected = edited_intake()$intake$tree[1], width = "100%"
        ),
        shiny::textInput(
          ns("website"), "Website:",
          value = edited_intake()$intake$website[1], width = "100%"
        )
      )
    })
    
    
    
    # Save course
    shiny::observeEvent(input$saveintake, {
      other_intakes <- course_data()$intakes |>
        dplyr::filter(intake != selected_intake)
      edtided <- tibble::tibble(
        intake = base::as.character(selected_intake),
        teachers = base::as.character(input$teachers),
        institution = base::as.character(input$institution),
        program = base::as.character(input$program),
        program_level = base::as.character(input$programlevel),
        year =  base::as.character(input$year),
        path = base::as.character(input$path),
        tree = base::as.character(input$tree),
        website = base::as.character(input$website)
      )
      intakes <- dplyr::bind_rows(edtided, other_intakes)
      base::save(intakes, file = course_paths()$databases$intakes)
      shinyalert::shinyalert(
        "Saved!",
        base::paste0("Changes to the course information have been saved. Reload the course to see changes."),
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
    
    
    output$students <- rhandsontable::renderRHandsontable({
      shiny::req(base::length(edited_intake()) == 2)
      edited_intake()$students |>
        rhandsontable::rhandsontable(rowHeaders = NULL, stretchH = "all") |>
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    
    
    shiny::observeEvent(input$savestudents, {
      studentlist <- rhandsontable::hot_to_r(input$students)
      if (base::nchar(input$newencryptkey) > 4){
        key <- input$newencryptkey
        studentlist <- studentlist |>
          teachR::encrypt_variable("firstname", key) |>
          teachR::encrypt_variable("lastname", key) |>
          teachR::encrypt_variable("email", key) |>
          teachR::encrypt_variable("studenturl", key)
      } else {
        studentlist <- studentlist
      }
      
      file <- base::paste0(
        course_paths()$subfolders$students,
        "/", edited_intake()$intake$intake, ".csv"
      )
      utils::write.csv(studentlist, file = file, row.names = FALSE)
      
      shinyalert::shinyalert(
        "Student list saved!",
        "If you applied an encryption key greater than 4 characters, the student list also has been encrypted.",
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
  })
}

