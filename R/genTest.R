#' @name genTest
#' @title Generate Tests
#' @author Nicolas Mangin
#' @description Shiny gadget to select test questions and generate both tests and solutions for several delivery formats.
#' @return Create an exam folder will all the the necessary files fo test administration and subsequent steps.
#' @importFrom miniUI miniPage
#' @importFrom miniUI gadgetTitleBar
#' @importFrom miniUI miniTabstripPanel
#' @importFrom miniUI miniTabPanel
#' @importFrom miniUI miniContentPanel
#' @importFrom shiny fillCol
#' @importFrom shiny fillRow
#' @importFrom shiny icon
#' @importFrom shiny fileInput
#' @importFrom shiny textInput
#' @importFrom shiny dateInput
#' @importFrom shiny numericInput
#' @importFrom shiny selectInput
#' @importFrom shiny checkboxInput
#' @importFrom shiny stopApp
#' @importFrom shiny runGadget
#' @importFrom shiny conditionalPanel
#' @importFrom shiny tags
#' @importFrom shiny tableOutput
#' @importFrom shiny dataTableOutput
#' @importFrom shiny uiOutput
#' @importFrom shiny actionButton
#' @importFrom shiny renderUI
#' @importFrom shiny renderText
#' @importFrom shiny renderTable
#' @importFrom shiny renderDataTable
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny withProgress
#' @importFrom shiny incProgress
#' @importFrom shiny withMathJax
#' @importFrom shiny dialogViewer
#' @importFrom shiny textOutput
#' @importFrom shiny htmlOutput
#' @importFrom shiny HTML
#' @importFrom shiny h4
#' @importFrom shinyBS bsTooltip
#' @importFrom shinythemes shinytheme
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom tidyr spread
#' @importFrom lubridate today
#' @importFrom exams exams2nops
#' @importFrom exams exams2pdf
#' @importFrom exams exams2moodle
#' @importFrom exams exams2blackboard
#' @importFrom exams exams2html
#' @importFrom purrr map
#' @importFrom xml2 read_html
#' @importFrom rvest html_node
#' @importFrom gtools permutations
#' @importFrom stats na.omit
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom rhandsontable hot_context_menu
#' @importFrom knitr knit
#' @export


genTest <- function() {
  ui <- miniPage(
    theme = shinythemes::shinytheme("flatly"),
    tags$head(tags$style(
      HTML(".shiny-notification {
              position:fixed;top: 30%;left: 0%;right: 0%;
           }")
    )),
    
    gadgetTitleBar("Test Generator"),
    
    miniTabstripPanel(
      
      #########################################################################
      
      miniTabPanel(
        "Information",
        icon = icon("id-card"),
        fillRow(
          flex = c(1,1),
          fillCol(
            flex = c(1,1,1,1,4),
            h4("General information about the test"),
            textInput(
              inputId = "institution",
              label = "Institution:",
              value = ""
            ),
            textInput(
              inputId = "course",
              label = "Course:",
              value = ""
            ),
            textInput(
              inputId = "name",
              label = "Name:",
              value = "test"
            ),
            tags$br()
          ),
          
          fillCol(
            flex = c(1,1,1,1,4),
            dateInput(
              inputId = "date",
              label = "Date:",
              value = lubridate::today()
            ),
            fileInput(
              "studentlist",
              "Upload a list of students:",
              accept = ".csv"
            ),
            fileInput(
              "preselection",
              "Upload a preselection of questions:",
              accept = ".csv"
            ),
            fileInput(
              "exclusion",
              "Upload a list of questions to exclude:",
              accept = ".csv"
            ),
            tags$br()
          )
        ),
        shinyBS::bsTooltip(
          id = "name",
          title = "Do not use spaces or special characters.",
          placement = "top", trigger = "hover"
        )
      ),
      
      #########################################################################
      
      miniTabPanel(
        "Delivery",
        icon = icon("paper-plane"),
        
        fillRow(
          flex = c(1,1,1),
          
          fillCol(
            flex = c(1,1,1,1,1,1,1,2),
            h4("Delivery format"),
            selectInput(
              "languages",
              "Select the language(s):",
              choices = c("DE","EN","ES","FR","IT","NL"),
              selected = "EN",
              multiple = TRUE
            ),
            selectInput(
              inputId = "currency",
              label = "Select the currency:",
              choices = c("euro", "dollar", "pound", "yen"),
              selected = "euro"
            ),
            checkboxInput(
              inputId = "showquestcode",
              label = "Show question code",
              value = TRUE
            ),
            checkboxInput(
              inputId = "showquestdiff",
              label = "Show question difficulty",
              value = TRUE
            ),
            checkboxInput(
              inputId = "showquestpt",
              label = "Show question points",
              value = TRUE
            ),
            selectInput(
              inputId = "typeanswer",
              label = "Type of answer:",
              choices = c("choice","number","text"),
              selected = "multiple-choice"
            ),
            conditionalPanel(
              condition = "input.typeanswer === 'choice'",
              numericInput(
                inputId = "alternatives",
                label = "Number of alternatives:",
                value = 5,
                min = 4,
                max = 5,
                step = 1
              )
            )
          ),
          fillCol(
            flex = c(1,6),
            h4("Delivery platforms"),
            fillRow(
              flex = c(1,1),
              fillCol(
                flex = c(1,1,1,1,1,1),
                checkboxInput("blackboard_out", "Blackboard", value = TRUE),
                checkboxInput("canvas_out", "Canvas", value = FALSE),
                checkboxInput("moodle_out", "Moodle", value = FALSE),
                checkboxInput("openolat_out", "openolat", value = FALSE),
                checkboxInput("arsnova_out", "arsnova", value = FALSE),
                checkboxInput("tcexam_out", "tcexam", value = FALSE)
              ),
              fillCol(
                flex = c(1,1,1,1,1,1),
                checkboxInput("pdf_out", "PDF", value = TRUE),
                checkboxInput("html_out", "html", value = FALSE),
                checkboxInput("pandoc_out", "Pandoc", value = FALSE),
                checkboxInput("nops_out", "nops", value = FALSE),
                checkboxInput("lops_out", "lops", value = FALSE),
                checkboxInput("qui12_out", "qti12", value = FALSE)
              )
            )
          ),
          
          fillCol(
            flex = c(1,1,1,6),
            h4("LMS details"),
            numericInput(
              inputId = "questversions",
              label = "Number of versions for each question:",
              value = 1,
              min = 1,
              max = 100,
              step = 1
            ),
            tags$hr(),
            conditionalPanel(
              h4("Print details"),
              condition = "input.pdf_out",
              selectInput(
                inputId = "format",
                label = "Format",
                choices = c("A4", "letter"),
                selected = "A4"
              ),
              checkboxInput(
                inputId = "withscan",
                label = "With scan (limit to 45 questions)",
                value = FALSE
              ),
              numericInput(
                inputId = "blocnbr",
                label = "Number of blocs",
                value = 0,
                min = 0,
                max = 10,
                step = 1
              ),
              numericInput(
                inputId = "blocsize",
                label = "Number of questions per bloc",
                value = 0,
                min = 0,
                max = 45,
                step = 1
              )
            )
          )
        ),
        shinyBS::bsTooltip(
          id = "languages",
          title = "Select only questions existing in all selected languages.",
          placement = "top", trigger = "hover"
        ),
        shinyBS::bsTooltip(
          id = "showquestcode",
          title = "Display as the beginning of each question a code identifying the question and its version.",
          placement = "top", trigger = "hover"
        ),
        shinyBS::bsTooltip(
          id = "showquestdiff",
          title = "Display at the end of the question whether the question is easy, medium, or hard.",
          placement = "top", trigger = "hover"
        ),
        shinyBS::bsTooltip(
          id = "showquestpt",
          title = "Display at the end of the question the number of points for that question.",
          placement = "top", trigger = "hover"
        ),
        shinyBS::bsTooltip(
          id = "typeanswer",
          title = "Select only questions compatible with the desired format: choice for MCQ, number for computations, text for open-ended answers.",
          placement = "top", trigger = "hover"
        ),
        shinyBS::bsTooltip(
          id = "alternatives",
          title = "This indicates among how many options the student must choose in MCQ.",
          placement = "top", trigger = "hover"
        ),
        shinyBS::bsTooltip(
          id = "questversions",
          title = "For a number greater than 1, several versions of each question will be generated.",
          placement = "top", trigger = "hover"
        ),
        shinyBS::bsTooltip(
          id = "withscan",
          title = "If the scan is requested, a scanable answer sheet is printed, limited to 45 answers.",
          placement = "top", trigger = "hover"
        ),
        shinyBS::bsTooltip(
          id = "blocnbr",
          title = "Different versions are created by shuffling blocs of questions.",
          placement = "top", trigger = "hover"
        ),
        shinyBS::bsTooltip(
          id = "blocsize",
          title = "The number of selected questions has to be equal to the number of blocs times the number of questions per bloc.",
          placement = "top", trigger = "hover"
        )
      ),
      
      #########################################################################
      
      miniTabPanel(
        "Selection",
        icon = icon("filter"),
        miniContentPanel(
          fillCol(
            flex = c(2,2,1,8),
            fillRow(
              flex = c(1,1,1,1,1),
              textInput("pkgname", "Package name:", value = ""),
              uiOutput("filtchapter"),
              uiOutput("filtsection"),
              uiOutput("filtsubsection"),
              uiOutput("filttopic")
            ),
            fillRow(
              flex = c(1,1,1,1,1),
              uiOutput("filttype"),
              uiOutput("filtlevel"),
              uiOutput("filtbloom"),
              uiOutput("filtdifficulty"),
              uiOutput("filtkeywords")
            ),
            tags$hr(),
            fillRow(
              flex = c(1,1),
              dataTableOutput("filtered_questions"),
              fillCol(
                flex = c(1,11),
                uiOutput("select_display"),
                uiOutput("lookexample")
              )
            )
          )
        ),
        shinyBS::bsTooltip(
          id = "pkgname",
          title = "From which package the next question should be taken?",
          placement = "top", trigger = "hover"
        )
      ),
      
      
      
      
      
      
      
      
      
      #########################################################################
      
      miniTabPanel(
        "Edit",
        icon = icon("edit"),
        miniContentPanel(
          
        )
      ),
      
      #########################################################################
      
      miniTabPanel(
        "Check",
        icon = icon("eye"),
        miniContentPanel(
          
        )
      ),
      
      #########################################################################
      
      miniTabPanel(
        "Balance",
        icon = icon("balance-scale"),
        miniContentPanel(
          
        )
      )
    )
  )
  

  server <- function(input, output, session) {
    
    # Bind variables for dplyr
    
    
    
    # Create reactive values
    tables <- reactiveValues()
    
    # Retrieve uploaded files
    observe({
      if (is.null(input$studentlist)) {
        tables$students_list <- data.frame(
          student_id = "",
          firstname = "",
          lastname = "",
          email = "",
          stringsAsFactors = FALSE
        )
      } else {
        tables$exclusion <- as.data.frame(
          utils::read.csv(
            file = input$studentlist$datapath[[1]],
            stringsAsFactors = FALSE
          ),
          stringsAsFactors = FALSE
        )
      }
    })
    
    observe({
      if (is.null(input$preselection)) {
        tables$preselection <- data.frame(
          package = "tmp",
          question_id = "tmp",
          language = "tmp",
          type = "tmp",
          question_number = 0,
          question_version = 0,
          question_code = "tmp",
          seed = 1,
          points = 1,
          difficulty = "tmp",
          stringsAsFactors = FALSE
        )
      } else {
        tables$preselection <- as.data.frame(
          utils::read.csv(
            file = input$selection$datapath[[1]],
            stringsAsFactors = FALSE
          ),
          stringsAsFactors = FALSE
        )
      }
    })
    
    observe({
      if (is.null(input$exclusion)) {
        tables$exclusion <- data.frame(
          question_id = "",
          stringsAsFactors = FALSE
        )
      } else {
        tables$exclusion <- as.data.frame(
          utils::read.csv(
            file = input$exclusion$datapath[[1]],
            stringsAsFactors = FALSE
          ),
          stringsAsFactors = FALSE
        )
      }
    })
    
    
    # Retrieve questions from package
    questionlist <- reactive({
      if (input$pkgname != ""){
        questions <- eval(
          parse(text = paste0(input$pkgname,"::","str_questions"))
        )
        
        # Selection of questions which are neither included nor excluded
        questions <- questions %>%
          dplyr::filter(!(question_id %in% tables$exclusion$question_id)) %>%
          dplyr::filter(!(question_id %in% tables$preselection$question_id))
        
        # Select questions available in appropriate languages
        
        in_all_languages <- questions %>%
          dplyr::select(question_nbr, question_language) %>%
          dplyr::filter(question_language %in% input$languages) %>%
          dplyr::group_by(question_nbr) %>%
          dplyr::count() %>%
          dplyr::filter(n == length(input$languages))
        
        questions <- questions %>%
          dplyr::filter(question_nbr %in% in_all_languages$question_nbr)
        
        # Selection question in appropriate format
        if (input$typeanswer == "choice") questions <- dplyr::filter(
          questions, type %in% c(
            "1 Statements", "2 Alternatives", "3 Assessement", "4 Computation"
          )
        )
        if (input$typeanswer == "number") questions <- dplyr::filter(
          questions, type %in% c("4 Computation")
        )
        if (input$typeanswer == "text") questions <- dplyr::filter(
          questions, type %in% c(
            "4 Computation", "5 Interrogation",
            "6 Problem", "7 Essay", "8 Case"
          )
        )
      }
      
      questions
    })
    
    
    ####################
    # Prepare filters
    base_filters <- reactive({
      filters <- eval(parse(text = paste0(input$pkgname,"::","str_labels")))
      
      # Selection question which are not excluded
      filters <- filters %>%
        dplyr::filter(language == input$languages[1]) %>%
        dplyr::right_join(
          questionlist(),
          by = c(
            "topic_id",
            "topic_order",
            "subsection_id",
            "subsection_order",
            "section_id",
            "section_order",
            "chapter_id",
            "chapter_order",
            "field_id",
            "field_label"
            )
          ) %>%
        tidyr::unite(
          "topic_code",
          field_id, chapter_order,
          section_order, subsection_order,
          topic_order,
          sep = ""
        ) %>%
        dplyr::select(
          question_id, question_nbr, topic_id, topic_code,
          chapter_label, section_label,
          subsection_label, topic_label,
          objective, description,
          type, level, bloom, difficulty
        ) %>%
        dplyr::arrange(topic_code)
    })
    
    
    
    
    # Create filters
    # Chapter
    output$filtchapter <- renderUI({
      if (input$pkgname != ""){
        make_filter(
          dataset = base_filters(), variable = "chapter_label",
          id = "slctchapter", label = "Chapter:"
        )
      }
    })
      
    afterfiltchapter <- reactive({
      if (input$pkgname != ""){
        filter_data(
          dataset = base_filters(), variable = "chapter_label",
          filt = input$slctchapter, type = "selection"
        )
      }
    })
    
    # Section
    output$filtsection <- renderUI({
      if (input$pkgname != ""){
        make_filter(
          dataset = afterfiltchapter(), variable = "section_label",
          id = "slctsection", label = "Section:"
        )
      }
    })
    afterfiltsection <- reactive({
      if (input$pkgname != ""){
        filter_data(
          dataset = afterfiltchapter(), variable = "section_label",
          filt = input$slctsection, type = "selection"
        )
      }
    })
    
    # Subsection
    output$filtsubsection <- renderUI({
      if (input$pkgname != ""){
        make_filter(
          dataset = afterfiltsection(), variable = "subsection_label",
          id = "slctsubsection", label = "Sub-section:"
        )
      }
    })
    afterfiltsubsection <- reactive({
      if (input$pkgname != ""){
        filter_data(
          dataset = afterfiltsection(), variable = "subsection_label",
          filt = input$slctsubsection, type = "selection"
        )
      }
    })
    
    # Topic
    output$filttopic <- renderUI({
      if (input$pkgname != ""){
        make_filter(
          dataset = afterfiltsubsection(), variable = "topic_label",
          id = "slcttopic", label = "Topic:"
        )
      }
    })
    afterfilttopic <- reactive({
      if (input$pkgname != ""){
        filter_data(
          dataset = afterfiltsubsection(), variable = "topic_label",
          filt = input$slcttopic, type = "selection"
        )
      }
    })
    
    # type
    output$filttype <- renderUI({
      if (input$pkgname != ""){
        make_filter(
          dataset = afterfilttopic(), variable = "type",
          id = "slcttype", label = "Question type:"
        )
      }
    })
    afterfilttype <- reactive({
      if (input$pkgname != ""){
        filter_data(
          dataset = afterfilttopic(), variable = "type",
          filt = input$slcttype, type = "selection"
        )
      }
    })
    
    # level
    output$filtlevel <- renderUI({
      if (input$pkgname != ""){
        make_filter(
          dataset = afterfilttype(), variable = "level",
          id = "slctlevel", label = "Level:"
        )
      }
    })
    afterfiltlevel <- reactive({
      if (input$pkgname != ""){
        filter_data(
          dataset = afterfilttype(), variable = "level",
          filt = input$slctlevel, type = "selection"
        )
      }
    })
    
    # bloom
    output$filtbloom <- renderUI({
      if (input$pkgname != ""){
        make_filter(
          dataset = afterfiltlevel(), variable = "bloom",
          id = "slctbloom", label = "Bloom level:"
        )
      }
    })
    afterfiltbloom <- reactive({
      if (input$pkgname != ""){
        filter_data(
          dataset = afterfiltlevel(), variable = "bloom",
          filt = input$slctbloom, type = "selection"
        )
      }
    })
    
    # Difficulty
    output$filtdifficulty <- renderUI({
      if (input$pkgname != ""){
        make_filter(
          dataset = afterfiltbloom(), variable = "difficulty",
          id = "slctdifficulty", label = "Difficulty:"
        )
      }
    })
    afterfiltdifficulty <- reactive({
      if (input$pkgname != ""){
        filter_data(
          dataset = afterfiltbloom(), variable = "difficulty",
          filt = input$slctdifficulty, type = "selection"
        )
      }
    })
    
    # Keywords
    output$filtkeywords <- renderUI({
      if (input$pkgname != ""){
        textInput(
          "slctkeywords",
          "Keywords (in objective or description):",
          value = "",
          width = "100%"
        )
      }
    })
    afterfiltkeywords <- reactive({
      if (input$pkgname != ""){
        inobj <- afterfiltdifficulty() %>%
          filter_data(
            variable = "objective",
            filt = input$slctkeywords,
            type = "text"
          )
        indesc <- afterfiltdifficulty() %>%
            filter_data(
            variable = "description",
            filt = input$slctkeywords,
            type = "text"
          )
        
        ineither <- unique(c(inobj$question_nbr), c(indesc$question_nbr))
        
        afterfiltdifficulty() %>%
          dplyr::filter(question_nbr %in% ineither)
      }
    })
    
    ############################################################################
    # Interface to see filtered questions
    output$filtered_questions <- shiny::renderDataTable({
      if (input$pkgname != ""){
        afterfiltkeywords() %>%
          dplyr::select(question_id, objective, description) %>%
          unique() %>%
          dplyr::arrange(question_id)
      }
    })
    
    output$select_display <- renderUI({
      if (input$pkgname != "") {
        choices <- afterfiltkeywords() %>%
          dplyr::select(question_id) %>%
          unique() %>%
          unlist() %>%
          as.character() %>%
          sort()
        selectInput(
          "slctdisp",
          "Select the question to display:",
          choices = choices,
          selected = choices[1]
        )
      }
    })
    
    output$lookexample <- renderUI({
      if (input$pkgname != "" & !is.null(input$slctdisp)){
        htmldoc <- system.file(
          "examples",
          paste0(input$slctdisp, ".html"),
          package="mancon"
        )
        page <- xml2::read_html(htmldoc)
        withMathJax(HTML(as.character(rvest::html_node(page, "body"))))
      }
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    output$edit_selection <- rhandsontable::renderRHandsontable({
      #rhandsontable::rhandsontable(tables$edit_paths, height = 400, width = "100%", stretchH = "all") %>%
      #  rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
    
    
    
    
    
    
    
    
    
    
    output$selected_questions <- renderTable({
      if (input$pkgname != "") afterfiltkeywords()
    })
    
    
    
    preselection <- reactive({
      reset <- input$addSample

      
    })
    

    ##########################################
    # Display tables

    output$manslct <- renderUI({
      if (!is.null(preselection())){
        preselection <- preselection()$questionid
        selectInput(
          inputId = "manslct",
          label = "Sample",
          choices = preselection,
          selected = preselection,
          multiple = TRUE,
          selectize = TRUE,
          width = "100%"
        )
      }
    })

    output$totalPoints <- renderText({
      paste0("Total points: ", sum(tables$contentexam$PT))
    })
    
    
    
    output$slctexample <- renderUI({
      if (!is.null(input$manslct)){
        
      }
    })
    
    
    
    
    ##############################
    
    output$selected_questions <- rhandsontable::renderRHandsontable({
      
    })
    
    
    output$checkblocs <- renderText({
      paste0("You need at least ", minquest, " questions or ", missing, " more question(s).")
    })
    
    
    ##############################
    
    output$viewquest <- renderUI({
      if (nrow(tables$contentexam) > 1){
        #questions <- tables$contentexam$QN
        examination <- c()
        for (question in questions){
          address <- system.file("documents", paste0(question, ".html"), package="questR")
          page <- xml2::read_html(address)
          examination[question] <- as.character(rvest::html_node(page, "body"))
        }
        withMathJax(HTML(examination))
      }
    })
    
    
    ##############################
    
    output$balance <- renderTable({
      
      
    })

    ##########################################
    # Actions to be executed on demand

    # Add the selected questions to the table
    observeEvent(input$addSample, {
      
    })

    
    

    #################
    # On exit

    observeEvent(input$done, {
      
      withProgress(message = "Making exam", value = 0, {
        
        choices <- tables$contentexam
        if (nrow(choices) > 45 & input$withscan) choices <- choices[1:45, ]
        
        # Save parameters accessible for question generation
        if (!is.null(input$typequest)) type_quest <- input$typequest else type_quest <- "mcq"
        if (type_quest == "mcq") {
          choices <- dplyr::filter(choices, KD == "mcq" | KD == "both")
        } else if (type_quest == "open") {
          choices <- dplyr::filter(choices, KD == "open" | KD == "both")
        }

        if (!is.null(input$showquestid)) show_question_id <- input$showquestid else show_question_id <- "Main"
        if (!is.null(input$showquestpt)) show_question_pt <- input$showquestpt else show_question_pt <- FALSE
        
        
        # Setup the exam folder
        if (dir.exists("examination")){
          wd <- paste0(getwd(), "/examination")
          setwd(wd)
        } else {
          dir.create("examination")
          wd <- paste0(getwd(), "/examination")
          setwd(wd)
          dir.create("1_students")
          dir.create("2_parameters")
          dir.create("tmp")
          dir.create("3_questions")
          
          
          dir.create("3_questions/pdf")
          dir.create("3_questions/blackboard")
          dir.create("3_questions/canvas")
          dir.create("3_questions/moodle")
          dir.create("3_questions/pandoc")
          dir.create("3_questions/nops")
          dir.create("3_questions/lops")
          dir.create("3_questions/html")
          dir.create("3_questions/openolat")
          dir.create("3_questions/arsnova")
          dir.create("3_questions/tcexam")
          dir.create("3_questions/qti12")
          
          
          
          
          dir.create("3_questions/web")
          dir.create("4_matrix")
          dir.create("5_reports")
          dir.create("6_feedback")
        }
        
        
        # Define the kind of table
        if (input$platform %in% c("Web", "Blackboard", "Moodle")) type_table <- "html" else type_table <- "latex"
        currency <- input$currency
        alternatives <- input$alternatives

        save(type_quest, show_question_id, show_question_pt, choices, type_table, currency, alternatives, file = paste0(wd, "/parameters/exam_parameters.RData"))

        # Set paths to exercises and templates
        exercises <- paste0(find.package("questR"), "/rmd")
        templates <- paste0(find.package("questR"), "/tex")

        # Create the paths for selected questions
        suffix <- ".Rmd"
        choices <- choices %>%
          dplyr::mutate(paths = purrr::map(QN, function(x, suffix) paste0(x, suffix), suffix = suffix))
        myexam <- choices$paths
        
        # Create the directory in which all files are generated
        ifelse(!dir.exists(file.path(wd, "tmp")), dir.create(file.path(wd, "tmp")), FALSE)
        tmpdir <- paste0(wd, "/tmp")
        unlink(paste0(tmpdir, "/*"))
        
        if (input$typequest == "mcq") stypequest <- "mcq" else stypequest <- "open"
        
        examid <- substring(gsub("-", "", input$datexam),3)


        # Generate the exam
        if (input$platform == "Web") {
          incProgress(1 / 2, detail = "Generating...")
          test_or_solution = "test"
          save(test_or_solution, file = paste0(wd, "/parameters/test_or_solution.RData"))
          web <- exams::exams2html(
            myexam,
            n = 1,
            dir = "web",
            edir = exercises,
            tdir = tmpdir,
            name = paste0(input$name, "_", examid),
            mathjax = TRUE
          )
          
        } else if (input$platform == "Blackboard") {
          incProgress(1 / 2, detail = "Generating...")
          test_or_solution = "test"
          save(test_or_solution, file = paste0(wd, "/parameters/test_or_solution.RData"))
          moodle <- exams::exams2blackboard(
            myexam,
            n = 1,
            dir = "blackboard",
            edir = exercises,
            tdir = tmpdir,
            zip = ifelse(Sys.info()[[1]] == "Windows", FALSE, TRUE),
            name = paste0(input$name, "_", examid)
          )
          
        } else if (input$platform == "Moodle") {
          incProgress(1 / 2, detail = "Generating...")
          test_or_solution = "test"
          save(test_or_solution, file = paste0(wd, "/parameters/test_or_solution.RData"))
          moodle <- exams2moodle(
            myexam,
            n = 1,
            dir = "moodle",
            edir = exercises,
            tdir = tmpdir,
            name = paste0(input$name, "_", examid)
          )
          
        } else {
          
          seed <- sample(c(1:9999), 1)
          steps <- 1 + input$versions
          
          incProgress(1 / steps, detail = "Preparing versions...")
          
          if (input$versions > 1) {
            numberblocs <- nrow(choices) / input$blocsize
            prepversions <- choices
            prepversions$bloc <- sort(rep(c(1:input$versions),input$blocsize))
            prepversions <- split(prepversions, prepversions$bloc)
            orderversions <- as.data.frame(t(gtools::permutations(numberblocs,numberblocs)))
            orderversions <- dplyr::select(orderversions, c("V1", sample(names(orderversions)[-1], (input$versions-1))))
            myexams <- list()
            for (i in 1:length(orderversions)){
              tmp <- prepversions[orderversions[,i]] %>%
                dplyr::bind_rows() %>%
                dplyr::mutate(version = i)
              myexams[[i]] <- tmp$paths
            }
            
          } else {
            myexams <- list(choices$paths)
          }
          
          for (i in 1:length(myexams)){
            
            incProgress((i+1) / steps, detail = paste0("Generating version ", i))
            
            # Set the exam id
            versionid <- paste0(
              examid,
              paste0(rep(0, (3 - nchar(as.character(i)))), collapse = ""),
              i
            )
            
            header <- list(Date = input$datexam, ID = versionid)
            
            test_or_solution = "test"
            save(test_or_solution, file = paste0(wd, "/parameters/test_or_solution.RData"))
            
            set.seed(seed)
            exam <- exams::exams2pdf(
              myexams[[i]],
              n = 1,
              dir = "print",
              edir = exercises,
              tdir = tmpdir,
              texdir = tmpdir,
              name = paste0("questions_", input$name, "_", versionid),
              header = header,
              points = choices$PT,
              template = paste0(templates, "/exam_", input$language, "_", input$typequest, "_", input$format, ".tex")
            )
            
            unlink(paste0(tmpdir, "/*"))
            
            test_or_solution = "solution"
            save(test_or_solution, file = paste0(wd, "/parameters/test_or_solution.RData"))
            
            set.seed(seed)
            solu <- exams::exams2pdf(
              myexams[[i]],
              n = 1,
              dir = "print",
              edir = exercises,
              tdir = tmpdir,
              texdir = tmpdir,
              name = paste0("solutions_", input$name, "_", versionid),
              header = header,
              template = paste0(templates, "/solution_", input$language, "_", input$typequest, "_", input$format, ".tex")
            )
            
            if (stypequest == "mcq" & input$withscan) {
              if (input$format == "A4") {
                teachR::make_scan_A4(
                  exam = exam,
                  name = versionid,
                  language = input$language,
                  title = input$title,
                  institution = input$institution,
                  date = input$datexam,
                  alternatives = input$alternatives,
                  reglength = input$reglength,
                  encoding = ""
                )
              }
            }
          }
          
        }

        exportexam <- choices %>%
          dplyr::select(-paths) %>%
          as.data.frame(stringsAsFactors = FALSE)
        
        utils::write.csv(exportexam, paste0("parameters/questions_", input$name, "_", examid, ".csv"), row.names = FALSE)
        
        if (input$versions > 1){
          exportblocs <- prepversions %>%
            dplyr::bind_rows() %>%
            dplyr::select(bloc, question = QN) %>%
            unique()
          
          utils::write.csv(exportblocs, paste0("parameters/blocs_", input$name, examid,".csv"), row.names = FALSE)
          utils::write.csv(orderversions, paste0("parameters/versions_", input$name, examid,".csv"), row.names = FALSE)
        }
        
      })

      stopApp()
    })
  }
  
  runGadget(ui, server, viewer = dialogViewer("genExam", width = 1900, height = 1080))
}






# Function to generate dynamic filters in user interface
make_filter <- function(dataset, variable, id, label) {
  choices <- sort(
    as.character(unique(c(unlist(dataset[, variable]), ""))),
    decreasing = FALSE
  )
  selectInput(
    id,
    label,
    choices = choices,
    selected = "",
    multiple = FALSE,
    width = "100%"
  )
}


# Functions to apply dynamically filters
filter_data <- function(dataset, variable, filt, type) {
  if (is.null(filt)) {
    dataset
  } else if (filt == "") {
    dataset
  } else {
    if (type == "selection") {
      dplyr::filter(dataset, str_detect(unlist(dataset[, variable]), filt))
    } else {
      terms <- stringr::str_to_lower(unlist(str_split(filt, " ")))
      terms <- stringr::str_replace_all(terms, "_", " ")
      base <- dataset
      for (term in terms) {
        base <- dplyr::filter(
          base,
          str_detect(stringr::str_to_lower(unlist(base[, variable])), term)
        )
      }
      base
    }
  }
}
