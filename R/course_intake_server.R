#' @name course_intake_server
#' @title Load intake data
#' @author Nicolas Mangin
#' @description Module facilitating the loading of an intake (with associated trees, paths, and tests).
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
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


course_intake_server <- function(id, course_data, course_paths){
  
  intake <- NULL
  jstrees <- NULL
  tbltrees <- NULL
  tree <- NULL
  path <- NULL
  courses <- NULL
  
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    output$intakepattern <- shiny::renderUI({
      shiny::req(!base::is.na(course_data()$intakes))
      shinyWidgets::searchInput(
          inputId = ns("defintakepattern"),
          label = "Preselect based on pattern:", 
          btnSearch = shiny::icon("search"), 
          btnReset = shiny::icon("remove"),
          width = "100%"
        )
    })
    
    output$slctintake <- shiny::renderUI({
      shiny::req(!base::is.na(course_data()$intakes))
      shiny::req(!base::is.null(input$defintakepattern))
      preslctintakes <- stats::na.omit(course_data()$intakes$intake)
      if (base::nchar(input$defintakepattern) > 0) {
        preslctintakes <- preslctintakes[stringr::str_detect(preslctintakes, input$defintakepattern)] |>
          base::sort()
      }
      shinyWidgets::pickerInput(
        inputId = ns("selectintake"),
        label = "Select an intake:", 
        choices = preslctintakes,
        selected = base::character(0),
        options = base::list(style = "btn-primary"),
        width = "100%"
      )
    })
    
    
    
    selected_intake <- shiny::reactive({
      shiny::req(!base::is.null(input$selectintake))
      intake <- dplyr::filter(course_data()$intakes, intake == input$selectintake)
      tbltree <- course_data()$tbltrees[[base::paste0(intake$tree[[1]], ".RData")]]
      jstree <- course_data()$jstrees[[base::paste0(intake$tree[[1]], ".RData")]]
      textbook <- classR::trees_structure_textbook(tbltree, intake$tree[1], intake$website[1])
      outcomes <- dplyr::filter(course_data()$outcomes, path == intake$website[1])
      connections <- dplyr::filter(course_data()$connections, path == intake$path[1])
      outlabels <- dplyr::filter(course_data()$outlabels, path == intake$path[1])
      activities <- dplyr::filter(course_data()$activities, path == intake$path[1])
      actlabels <- dplyr::filter(course_data()$actlabels, path == intake$path[1])
      attributes <- dplyr::filter(course_data()$attributes, path == intake$path[1])
      
      studentfile <- base::paste0(
        course_paths()$subfolders$students,
        "/", intake$intake, ".csv"
      )
      if (base::file.exists(studentfile)){
        students <- utils::read.csv(studentfile, colClasses = "character")
      }
      
      base::list(
        intake = intake,
        tbltree = tbltree,
        jstree = jstree,
        textbook = textbook,
        outcomes = outcomes,
        connections = connections,
        outlabels = outlabels,
        activities = activities,
        actlabels = actlabels,
        attributes = attributes,
        students = students
      )
    })
    
    shiny::observe({ selected_intake() })
    
    
    
    shiny::observeEvent(input$newintake, {
      if (base::length(course_data()$courses) == 1){
        shinyalert::shinyalert(
          "Load a course first!",
          "You need to load a course to create a new intake.",
          type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      } else {
        
        intakes <- course_data()$intakes
        last_intake <- intakes[1,]
        allpaths <- base::unique(course_data()$outcomes$path)
        alltrees <- stringr::str_remove_all(base::names(course_data()$tbltrees), ".RData")
        year <- base::substr(base::Sys.Date(),1,4)
        
        shiny::showModal(
          shiny::modalDialog(
            style = "background-color:#001F3F;color:#FFF;margin-top:300px;",
            shiny::textInput(
              ns("newintakename"), "Save intake as:",
              value = "new_intake", width = "100%"
            ),
            shiny::textInput(
              ns("newintaketeachers"), "Authors:",
              value = last_intake$teachers[1], width = "100%"
            ),
            shiny::selectizeInput(
              ns("newintakeinstitution"), "Institution:",
              choices = base::unique(intakes$institution),
              selected = last_intake$institution[1],
              width = "100%", options = base::list(create = TRUE)
            ),
            shiny::selectizeInput(
              ns("newintakeprogram"), "Program:",
              choices = base::unique(intakes$program),
              selected = last_intake$program[1],
              width = "100%", options = base::list(create = TRUE)
            ),
            shiny::selectizeInput(
              ns("newintakeprogramlevel"), "Program level:",
              choices = base::unique(intakes$program_level),
              selected = last_intake$program_level[1],
              width = "100%", options = base::list(create = TRUE)
            ),
            shiny::numericInput(
              ns("newintakeyear"), "Year:", value = year, width = "100%"
            ),
            shiny::selectInput(
              ns("newintakepath"), "Path:", choices = allpaths,
              selected = last_intake$path[1], width = "100%"
            ),
            shiny::selectInput(
              ns("newintaketree"), "Tree:", choices = alltrees,
              selected = last_intake$tree[1], width = "100%"
            ),
            shiny::textInput(
              ns("newintakewebsite"), "Website:",
              value = last_intake$website[1], width = "100%"
            ),
            footer = tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(
                ns("makenewintake"), "OK",
                icon = shiny::icon("check"),
                style = "background-color:#007777;color:#FFF;"
              )
            )
          )
        )
      }
    })
    
    shiny::observeEvent(input$makenewintake, {
      
      if (input$newintakename %in% course_data()$intakes$intake){
        
        shinyalert::shinyalert(
          "Intake already existing!",
          "This name has already been attributed to an existing intake. Please rename.",
          type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
        
      } else {
        
        shiny::removeModal()
        
        addintake <- tibble::tibble(
          intake = input$newintakename,
          teachers = input$newintaketeachers,
          institution = input$newintakeinstitution,
          program = input$newintakeprogram,
          program_level = input$newintakeprogramlevel,
          year =  input$newintakeyear,
          path =  input$newintakepath,
          tree =  input$newintaketree,
          website = input$newintakewebsite
        ) |>
          dplyr::mutate(year = base::as.character(year))
        intakes <- dplyr::bind_rows(addintake, course_data()$intakes)
        base::save(intakes, file = course_paths()$databases$intakes)
        
        studentlist <- utils::read.csv(
          base::paste0(course_paths()$subfolders$students, "/init.csv")
        )
        file <- base::paste0(
          course_paths()$subfolders$students,
          "/", input$newintakename, ".csv"
        )
        utils::write.csv(studentlist, file = file, row.names = FALSE)
        
        shinyalert::shinyalert(
          "Intake created!",
          "The new intake has been created. Reload the course to enact changes.",
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
        
      }
    })
    
    
    
    output$editintake <- shiny::renderUI({
      shiny::req(!base::is.na(selected_intake()$intake$intake))
      
      allpaths <- base::unique(course_data()$outcomes$path)
      alltrees <- stringr::str_remove_all(base::names(course_data()$tbltrees), ".RData")
      
      base::list(
        shiny::h4(selected_intake()$intake$intake[1]),
        shiny::textInput(
          ns("teachers"), "Teachers:",
          value = selected_intake()$intake$teachers[1], width = "100%"
        ),
        shiny::textInput(
          ns("institution"), "Institution:",
          value = selected_intake()$intake$institution[1], width = "100%"
        ),
        shiny::textInput(
          ns("program"), "Program:",
          value = selected_intake()$intake$program[1], width = "100%"
        ),
        shiny::textInput(
          ns("programlevel"), "Program level:",
          value = selected_intake()$intake$program_level[1], width = "100%"
        ),
        shiny::numericInput(
          ns("year"), "Year:",
          value = selected_intake()$intake$year[1], width = "100%"
        ),
        shiny::selectInput(
          ns("path"), "Path:", choices = allpaths,
          selected = selected_intake()$intake$path[1], width = "100%"
        ),
        shiny::selectInput(
          ns("tree"), "Tree:", choices = alltrees,
          selected = selected_intake()$intake$tree[1], width = "100%"
        ),
        shiny::textInput(
          ns("website"), "Website:",
          value = selected_intake()$intake$website[1], width = "100%"
        )
      )
    })
    
    
    
    # Save course
    shiny::observeEvent(input$saveintake, {
      other_intakes <- course_data()$intakes |>
        dplyr::filter(intake != input$selectintake)
      edtided <- tibble::tibble(
        intake = base::as.character(input$selectintake),
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
      shiny::req(!base::is.na(selected_intake()$students))
      selected_intake()$students |>
        rhandsontable::rhandsontable(rowHeaders = NULL, stretchH = "all") |>
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    
    
    shiny::observeEvent(input$savestudents, {
      studentlist <- rhandsontable::hot_to_r(input$students)
      file <- base::paste0(
        course_paths()$subfolders$students,
        "/", selected_intake()$intake$intake, ".csv"
      )
      utils::write.csv(studentlist, file = file, row.names = FALSE)
    })
    
    
    
    return(selected_intake)
  })
}

