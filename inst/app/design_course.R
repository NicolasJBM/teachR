design_course <- shiny::shinyApp(

  # INTERFACE ##################################################################

  ui = shinydashboardPlus::dashboardPage(

    options = base::list(sidebarExpandOnHover = TRUE),

    # HEADER ###################################################################

    header = shinydashboardPlus::dashboardHeader(
      fixed = FALSE,
      leftUi = shiny::tagList(

        shiny::icon("creative-commons"),
        shiny::icon("creative-commons-by"),
        shiny::icon("creative-commons-sa"),

        shiny::tags$button(
          id = "exit", type = "button", class = "btn action-button",
          onclick = "setTimeout(function(){window.close();},100);",
          style = "background-color:#660033;color:#FFF;width:150px;border:0px;",
          shiny::icon("power-off"),
          shiny::span("Exit", title = "Exit the application without saving your unsaved work.")
        )

      )
    ),

    # SIDEBAR ##################################################################

    # Menus are only displayed if the corresponding header switch is TRUE
    sidebar = shinydashboardPlus::dashboardSidebar(
      minified = TRUE, collapsed = TRUE, width = 230,
      shinydashboard::sidebarMenu(
        
        shinydashboard::menuItem(
          shiny::span("Writing", title = "Write and translate different types of teaching materials: documents like slide presentations, scripts for videos, pages of textbooks, papers for blogs, test questions or supporting functions, templates, diagrams etc. You can also edit the propositions and answers included in questions and relative to the specific document."),
          tabName = "writing", icon = shiny::icon("feather")
        ),
        shinydashboard::menuItem(
          shiny::span("Organizing", title = "Create and curate tags for documents, classification trees, and learning paths."),
          tabName = "organizing", icon = shiny::icon("folder-tree")
        ),
        shinydashboard::menuItem(
          shiny::span("Testing", title = "Create tests for formative or summative assessments from the question bank."),
          tabName = "testing", icon = shiny::icon("file-circle-question")
        ),
        
        shiny::tags$hr(),
        
        shinydashboard::menuItem(
          shiny::span("Enrolling", title = "Create intakes, define their properties, and associate them with a tree, a path, as well as an encrypted list of students."),
          tabName = "enrolling", icon = shiny::icon("users")
        ),
        shinydashboard::menuItem(
          shiny::span("Grading", title = "Check students' answers, grade their work, adapt solutions, check grading consistency and send feedback."),
          tabName = "grading", icon = shiny::icon("file-circle-check")
        ),
        shinydashboard::menuItem(
          shiny::span("Analyzing", title = "Analyze teaching materials or students performance."),
          tabName = "analyzing", icon = shiny::icon("heart-pulse")
        )
      )
    ),

    # BODY #####################################################################

    body = shinydashboard::dashboardBody(
      shiny::tags$head(
        shiny::tags$head(
          shiny::tags$style(shiny::HTML('
            .jstree-rename-input {
              max-height:25px !important;
            }
            .main-header .logo {
              height: 60px;
            }
            .sidebar-menu {
              padding-top: 10px;
            }
          ')),
          shiny::tags$style(".modal-dialog{width:90%}")
        )
      ),

      shiny::tags$script(src = "https://unpkg.com/panzoom@9.4.3/dist/panzoom.min.js"),
      shiny::tags$script(shiny::HTML("$('body').addClass('fixed');")),

      shinydashboard::tabItems(

        # Organization #########################################################

        shinydashboard::tabItem(
          tabName = "organizing",  shiny::tags$br(),
          
          shinydashboard::tabBox(
            side = "left", width = "100%",
            shiny::tabPanel(
              title = shiny::span(
                shiny::icon("sitemap"), "Trees",
                title = "Classify documents in a hierarchical tree which can be used for document selection and textbook publication."
              ),
              classR::trees_edit_ui("edittree")
            ),
            shiny::tabPanel(
              title = shiny::span(
                shiny::icon("tags"), "Tags",
                title = "List and curate tags, organize their respective categories, and batch-edit them in documents."
              ),
              classR::tags_edit_ui("edittags")
            ),
            shiny::tabPanel(
              title = shiny::span(
                shiny::icon("timeline"), "Path",
                title = "Organize teaching materials in a learning journey for students.",
              ),
              pathR::design_path_ui("despath")
            )
          )
        ),

        # Writing ##############################################################

        shinydashboard::tabItem(
          tabName = "writing", shiny::tags$br(),
          shinydashboard::tabBox(
            side = "left", width = "100%",
            shiny::tabPanel(
              title = shiny::span(
                shiny::icon("images"), "Presentations",
                title = "Presentations are a sets of slides constituting a single document in a revealJS format which can be uploaded online."
              ),
              editR::edit_ui("editpresentations")
            ),
            shiny::tabPanel(
              title = shiny::span(
                shiny::icon("video"), "Scripts",
                title = "A script for a video. What is written as a quote is exported in a .txt file for a prompter. The rest is description of paces and intonations or of visuals."
              ),
              editR::edit_ui("editscripts")
            ),
            shiny::tabPanel(
              title = shiny::span(
                shiny::icon("book-open"), "Pages",
                title = "A page is web-page meant to be inserted in a classification tree (in 'prepare') to be published in a textbook. It is not a stand-alone document."
              ),
              editR::edit_ui("editpages")
            ),
            shiny::tabPanel(
              title = shiny::span(
                shiny::icon("newspaper"), "Papers",
                title = "A paper is a stand-alone webpage which can be inserted in a blog as a post."
              ),
              editR::edit_ui("editpapers")
            ),
            shiny::tabPanel(
              title = shiny::span(
                shiny::icon("circle-question", "fa-solid"), "Questions",
                title = "A question is a stand-alone question which can be selected in tests in which all questions are independent from each other."
              ),
              editR::edit_ui("editquest")
            ),
            shiny::tabPanel(
              title = shiny::span(
                shiny::icon("language", "fa-solid"), "Translate",
                title = "Translate a document in different languages."
              ),
              editR::translate_ui("translation")
            ),
            shiny::tabPanel(
              title = shiny::span(
                shiny::icon("code"), "Code",
                title = "Code custom functions or templates used in multiple documents.",
              ),
              editR::code_edit_ui("editcode")
            ),
            shiny::tabPanel(
              title = shiny::span(
                shiny::icon("diagram-project"), "Diagrams",
                title = "Draw diagrams and flowcharts for subsequent insertion in documents."
              ),
              chartR::edit_diagram_ui("ediag")
            ),
            shiny::tabPanel(
              title = shiny::span(
                shiny::icon("graduation-cap"), "References",
                title = "Batch import references or identify and remove duplicated references."
              ),
              bibliogR::manage_references_ui("manageref")
            )
          )
        ),

        # Test #################################################################

        shinydashboard::tabItem(
          tabName = "testing", shiny::tags$br(),
          shiny::tags$hr(),
          testR::edit_test_ui("editest")
        ),

        # Intake #################################################################
        
        shinydashboard::tabItem(
          tabName = "enrolling", shiny::tags$br(),
          teachR::course_intake_ui("coursetree")
        ),
        
        # Grade ################################################################

        shinydashboard::tabItem(
          tabName = "grading", shiny::tags$br(),
          gradR::grading_ui("grading")
        ),
        
        # Analyze ##############################################################

        shinydashboard::tabItem(
          tabName = "analyzing", shiny::tags$br()#,
          #reportR::analysis_ui("analysis")
        )

      )
    ),

    # CONTROLS #################################################################

    controlbar = shinydashboardPlus::dashboardControlbar(
      id = "rightsidebar", width = 550, collapsed = FALSE, overlay = TRUE,
      shinydashboardPlus::controlbarMenu(
        id = "controlbar",

        shinydashboardPlus::controlbarItem(
          title = shiny::span("Course", title = "Select the tree, path, and intake and update documents."),
          icon = shiny::icon("map"),
          shiny::tags$br(),
          shiny::actionButton(
            "newcourse", "Create course", icon = shiny::icon("wand-magic-sparkles"),
            style = "background-color:#006633;color:#FFF;width:100%;border:0px;margin-left:10px;"
          ),
          shiny::tags$hr(),
          shiny::fluidRow(
            shiny::column(
              6,
              shinyWidgets::pickerInput(
                "coursefolder", "Course folder:",
                choices = "", selected = "",
                inline = TRUE, width = "100%"
              )
            ),
            shiny::column(
              6,
              shiny::actionButton(
                "opencoursefolder", "Open folder", icon = shiny::icon("folder-open"),
                style = "background-color:#330066;color:#FFF;width:100%;border:0px;margin-top:25px;"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::actionButton(
                "updatedoctagtree", "Update documents", icon = shiny::icon("layer-group"),
                style = "background-color:#000099;color:#FFF;width:100%;border:0px;"
              )
            ),
            shiny::column(
              4,
              shiny::actionButton(
                "updatetestsol", "Update tests", icon = shiny::icon("list-check"),
                style = "background-color:#000099;color:#FFF;width:100%;border:0px;"
              )
            ),
            shiny::column(
              4,
              shiny::actionButton(
                "updatepaths", "Update paths", icon = shiny::icon("diagram-project"),
                style = "background-color:#000099;color:#FFF;width:100%;border:0px;"
              )
            )
          ),
          shiny::tags$br(),
          teachR::course_load_ui("loadcourse"),
          shiny::tags$hr(),
          shiny::uiOutput("selecttree"),
          shiny::uiOutput("selectpath"),
          shiny::uiOutput("selectintake")
        ),
        
        shinydashboardPlus::controlbarItem(
          title = shiny::span("Tree", title = "Filter documents based on a tree structure."),
          icon = shiny::icon("sitemap"),
          teachR::filter_tree_ui("filttree"),
          shiny::tags$br(), shiny::textOutput("countaftertree")
        ),

        shinydashboardPlus::controlbarItem(
          title = shiny::span("Tags", title = "Filter documents based on tags."),
          icon = shiny::icon("tags"),
          teachR::filter_tags_ui("filttags"),
          shiny::tags$br(), shiny::textOutput("countaftertags")
        ),

        shinydashboardPlus::controlbarItem(
          title = shiny::span("Statistics", title = "Select documents based on statistics."),
          icon = shiny::icon("percent"),
          #teachR::filter_statistics_ui("filtstat"),
          shiny::tags$br(), shiny::textOutput("countafterstats")
        ),

        shinydashboardPlus::controlbarItem(
          title = shiny::span("Languages", title = "Filter document based on languages."),
          icon = shiny::icon("language"),
          teachR::filter_languages_ui("filtlang"),
          shiny::tags$br(), shiny::textOutput("countafterlang")
        ),

        shinydashboardPlus::controlbarItem(
          title = shiny::span("References", title = "Find references to cite."),
          icon = shiny::icon("graduation-cap"),
          bibliogR::edit_references_ui("editref"),
          shiny::tags$br(),
          bibliogR::search_references_ui("filtref")
        )

      )
    )
  ),

  # SERVER #####################################################################

  server = function(session, input, output) {

    base::options(
      scipen = 100,
      shiny.maxRequestSize=300*1024^2
    )
    library("jsTreeR")
    library("teachR")
    library("editR")
    library("bibliogR")
    library("chartR")

    # Course selections ########################################################

    reactval <- shiny::reactiveValues()
    
    # Select a course
    
    courses <- shiny::reactive({
      pathfile <- base::paste0(base::path.package("teachR"), "/app/mainpath.txt")
      if (base::file.exists(pathfile)) {
        path <- tibble::tibble(line = base::readLines(pathfile)) |>
          dplyr::mutate(exists = purrr::map_lgl(line, dir.exists)) |>
          dplyr::filter(exists == TRUE) |>
          dplyr::slice_head(n = 1) |> dplyr::select(line) |>
          base::unlist() |> base::as.character()  
        base::setwd(path)
        tibble::tibble(
          name = base::list.dirs(path, full.names = FALSE, recursive = FALSE),
          path = base::list.dirs(path, full.names = TRUE, recursive = FALSE)
        )
      } else NA
    })

    shiny::observe({
      if (base::length(courses()) > 1) {
        predefined_course_folders <- courses()$path
        base::names(predefined_course_folders) <- courses()$name
        shinyWidgets::updatePickerInput(
          session,
          "coursefolder",
          choices = predefined_course_folders,
          selected = predefined_course_folders[1]
        )
      }
    })
    
    course_folder <- shiny::reactive({
      shiny::req(!base::is.null(input$coursefolder))
      if (base::file.exists(base::paste0(input$coursefolder, "/subfolders.csv"))){
        course_folder <- input$coursefolder
      } else {
        course_folder <- "None"
      }
      course_folder
    })

    shiny::observeEvent(input$opencoursefolder, {
      if (base::dir.exists(course_folder())){
        if (.Platform['OS.type'] == "windows"){
          shell.exec(course_folder())
        } else {
          system2("open", course_folder())
        }
      }
    })

    course_paths <- shiny::reactive({
      if (course_folder() != "None"){
        teachR::set_course_paths(course_folder())
      } else base::list(NA)
    })
    
    # Load course databases and select contents
    course_data <- shiny::reactive({
      teachR::course_load_server("loadcourse", course_paths)
    })
    
    intakes <- shiny::reactive({
      shiny::req(!base::is.logical(course_data()$intakes))
      course_data()$intakes |>
        dplyr::select(intake, path, tree) |>
        base::unique()
    })
    
    output$selecttree <- shiny::renderUI({
      shiny::req(!base::is.null(intakes()))
      shiny::req(base::nrow(intakes()) > 0)
      shinyWidgets::pickerInput(
        inputId = "slcttree",
        label = "Tree:", 
        choices = base::unique(intakes()$tree),
        width = "100%"
      )
    })
    
    output$selectpath <- shiny::renderUI({
      shiny::req(!base::is.null(input$slcttree))
      preslctintakes <- course_data()$intakes |>
        dplyr::filter(tree == input$slcttree)
      shinyWidgets::pickerInput(
        inputId = "slctpath",
        label = "Path:", 
        choices = base::unique(preslctintakes$path),
        width = "100%"
      )
    })
    
    output$selectintake <- shiny::renderUI({
      shiny::req(!base::is.null(input$slctpath))
      preslctintakes <- course_data()$intakes |>
        dplyr::filter(path == input$slctpath)
      reactval$path <- input$slctpath
      shinyWidgets::pickerInput(
        inputId = "slctintake",
        label = "Intake:", 
        choices = base::unique(preslctintakes$intake),
        width = "100%"
      )
    })
    
    jstree <- shiny::reactive({
      shiny::req(!base::is.null(input$slcttree))
      treefile <- base::paste0(input$slcttree, ".RData")
      shiny::req(treefile %in% base::names(course_data()$jstrees))
      course_data()$jstrees[[treefile]]
    })
    
    tbltree <- shiny::reactive({
      shiny::req(!base::is.null(input$slcttree))
      treefile <- base::paste0(input$slcttree, ".RData")
      shiny::req(treefile %in% base::names(course_data()$tbltrees))
      course_data()$tbltrees[[treefile]]
    })
    
    #path <- shiny::reactive({
    #  # Interupted warning happens here for some unknwon reason
    #  shiny::req(!base::is.null(input$slctpath))
    #  input$slctpath
    #})
    
    
    # Update course databases
    shiny::observeEvent(input$updatedoctagtree, {
      if (base::length(course_paths()) != 2){
        shinyalert::shinyalert(
          title = "Please select a course folder.",
          text = "You must first select a course folder to perform this action.",
          type = "error"
        )
      } else {
        shinybusy::show_modal_progress_line(value = 0/30, text = "Updating documents")
        teachR::update_documents(course_paths())
        shinybusy::show_modal_progress_line(value = 10/30, text = "Updating tags")
        teachR::update_tags(course_paths())
        shinybusy::show_modal_progress_line(value = 20/30, text = "Updating trees")
        teachR::update_trees(course_paths())
        shinybusy::remove_modal_spinner()
        shinyalert::shinyalert(
          title = "Documents, tags, and trees updated!",
          text = "Reload the course to see the changes.",
          type = "success"
        )
      }
    })
    
    shiny::observeEvent(input$updatetestsol, {
      if (base::length(course_paths()) != 2){
        shinyalert::shinyalert(
          title = "Please select a course folder.",
          text = "You must first select a course folder to perform this action.",
          type = "error"
        )
      } else {
        shinybusy::show_modal_progress_line(value = 0/20, text = "Updating tests")
        teachR::update_tests(course_paths())
        shinybusy::show_modal_progress_line(value = 10/20, text = "Updating solutions")
        teachR::update_solutions(course_paths())
        shinybusy::remove_modal_spinner()
        shinyalert::shinyalert(
          title = "Tests and solutions updated!",
          text = "Reload the course to see the changes.",
          type = "success"
        )
      }
    })
    
    shiny::observeEvent(input$updatepaths, {
      if (base::length(course_paths()) != 2){
        shinyalert::shinyalert(
          title = "Please select a course folder.",
          text = "You must first select a course folder to perform this action.",
          type = "error"
        )
      } else {
        shinybusy::show_modal_progress_line(value = 0/10, text = "Updating paths")
        teachR::update_paths(course_paths())
        shinybusy::remove_modal_spinner()
        shinyalert::shinyalert(
          title = "Learning paths updated!",
          text = "Reload the course to see the changes.",
          type = "success"
        )
      }
    })

    teachR::course_update_server("updatecourse", course_paths)
    
    # Document selection #######################################################

    # Tree
    selected_from_tree <- teachR::filter_tree_server("filttree", jstree, course_data)
    
    # Tags
    selected_from_tags <- teachR::filter_tags_server("filttags", course_data)
    
    # Statistics
    #selected_from_stats <- teachR::filter_statistics_server("filtstat", course_data)

    # Languages
    selected_from_lang <- teachR::filter_languages_server("filtlang", course_data)
    
    # Intersection of selections
    filtered_documents <- shiny::reactive({
      shiny::req(!base::is.na(course_data()))
      shiny::req(!base::is.null(selected_from_tree()))
      shiny::req(!base::is.null(selected_from_tags()))
      #shiny::req(!base::is.null(selected_from_stats()))
      shiny::req(!base::is.null(selected_from_lang()))
      selected_from_tree() |>
        dplyr::inner_join(selected_from_tags(), by = "file") |>
        #dplyr::inner_join(selected_from_stats(), by = "file") |>
        dplyr::inner_join(selected_from_lang(), by = "file") |>
        dplyr::inner_join(course_data()$documents, by = "file") |>
        dplyr::left_join(dplyr::select(tbltree(), file, position), by = "file") |>
        dplyr::arrange(position) |>
        dplyr::select(-position)
    })

    output$countaftertree <- shiny::renderText({
      shiny::req(!base::is.null(filtered_documents()))
      teachR::filter_documents_count(filtered_documents())
    })
    output$countaftertags <- shiny::renderText({
      shiny::req(!base::is.null(filtered_documents()))
      teachR::filter_documents_count(filtered_documents())
    })
    output$countafterstats <- shiny::renderText({
      shiny::req(!base::is.null(filtered_documents()))
      teachR::filter_documents_count(filtered_documents())
    })
    output$countafterlang <- shiny::renderText({
      shiny::req(!base::is.null(filtered_documents()))
      teachR::filter_documents_count(filtered_documents())
    })

    references <- bibliogR::edit_references_server(
      "editref", course_paths, base::getwd()
    )

    bibliogR::search_references_server("filtref", references)

    bibliogR::manage_references_server("manageref", references, base::getwd())

    # Prepare ##################################################################

    editR::code_edit_server("editcode", course_paths)

    chartR::edit_diagram_server("ediag", course_paths()$subfolders$databases)

    # Edit documents ###########################################################
    
    editR::edit_server(
      "editpresentations", filtered = filtered_documents, course_data = course_data,
      tree = input$slcttree, tbltree = tbltree, course_paths = course_paths, doctype = "Presentation"
    )
    
    editR::edit_server(
      "editscripts", filtered = filtered_documents, course_data = course_data,
      tree = input$slcttree, tbltree = tbltree, course_paths = course_paths, doctype = "Script"
    )
    
    editR::edit_server(
      "editpages", filtered = filtered_documents, course_data = course_data,
      tree = input$slcttree, tbltree = tbltree, course_paths = course_paths, doctype = "Page"
    )
    
    editR::edit_server(
      "editpapers", filtered = filtered_documents, course_data = course_data,
      tree = input$slcttree, tbltree = tbltree, course_paths = course_paths, doctype = "Paper"
    )

    editR::edit_server(
      "editquest", filtered = filtered_documents, course_data = course_data,
      tree = input$slcttree, tbltree = tbltree, course_paths = course_paths, doctype = "Question"
    )

    # Translate ################################################################

    editR::translate_server(
      "translation", filtered = filtered_documents, course_data = course_data,
      tree = input$slcttree, tbltree = tbltree, course_paths = course_paths
    )

    # Organize #################################################################

    classR::trees_edit_server(
      "edittree",
      tree = input$slcttree,
      jstree = jstree,
      course_data,
      course_paths
    )
    
    classR::tags_edit_server(
      "edittags",
      course_data,
      course_paths
    )
    
    pathR::design_path_server(
      "despath",
      selected_path = reactval$path,
      tbltree = tbltree,
      course_data = course_data,
      course_paths = course_paths
    )

    # Create tests #############################################################

    #testR::edit_test_server(
    #  "editest", filtered = filtered_documents, course_data = course_data,
    #  intake = input$slctintake, course_paths = course_paths
    #)

    # Define intakes ###########################################################
    
    #teachR::course_intake_server("coursetree", input$slctintake, course_data, course_paths)
    
    # Grade tests ##############################################################

    #gradR::grading_server("grading", course_data, course_paths)

    # Feedback #################################################################

    #reportR::feedback_server("testfeedback", test, tree, course_data, course_paths)

    # Analyze courses and students #############################################

    #reportR::analysis_server("analysis", course_paths, references)

    # Exit the application #####################################################

    shiny::observeEvent(input$exit, {
      teachR::course_clean_root()
      shiny::stopApp()
    })
  }
)
