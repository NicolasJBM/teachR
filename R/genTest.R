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
#' @importFrom shiny renderPlot
#' @importFrom shiny plotOutput
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
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom lubridate today
#' @importFrom tibble rownames_to_column
#' @importFrom exams exams2nops
#' @importFrom exams exams2pdf
#' @importFrom exams exams2moodle
#' @importFrom exams exams2blackboard
#' @importFrom exams exams2html
#' @importFrom purrr map
#' @importFrom purrr map2
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
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 coord_flip
#' @export


genTest <- function() {
  ui <- miniPage(
    theme = shinythemes::shinytheme("flatly"),
    tags$head(tags$style(
      HTML(".shiny-notification {
              position:fixed;top: 30%;left: 0%;right: 0%;
           }
           .flexfill-item {
              margin:5px 10px 5px 10px !important;
              font-size:0.9em !important;
           }
           .gadget-tabs-content-container {
              margin:1em 1em 1em 1em !important;
           }")
    )),
    
    gadgetTitleBar("Test Generator"),
    
    miniTabstripPanel(
      
      ##########################################################################
      
      miniTabPanel(
        "Information",
        icon = icon("id-card"),
        h4("General information about the test"),
        fillRow(
          flex = c(1,1),
          fillCol(
            flex = c(1,1,1,4),
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
            textInput(
              inputId = "prefix",
              label = "Question prefix:",
              value = "pre"
            )
          ),
          
          fillCol(
            flex = c(1,1,1,4),
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
            )
          )
        ),
        shinyBS::bsTooltip(
          id = "name",
          title = "Do not use spaces or special characters.",
          placement = "top", trigger = "hover"
        ),
        shinyBS::bsTooltip(
          id = "prefix",
          title = "String appended at the beginning of the question code.",
          placement = "top", trigger = "hover"
        )
      ),
      
      ##########################################################################
      
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
              inputId = "showexname",
              label = "Show question code",
              value = TRUE
            ),
            checkboxInput(
              inputId = "showquestdifficulty",
              label = "Show question difficulty",
              value = TRUE
            ),
            checkboxInput(
              inputId = "showquestpoint",
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
          id = "showexname",
          title = "Display as the beginning of each question a code identifying the question and its version.",
          placement = "top", trigger = "hover"
        ),
        shinyBS::bsTooltip(
          id = "showquestdifficulty",
          title = "Display at the end of the question whether the question is easy, medium, or hard.",
          placement = "top", trigger = "hover"
        ),
        shinyBS::bsTooltip(
          id = "showquestpoint",
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
      
      ##########################################################################
      
      miniTabPanel(
        "Selection",
        icon = icon("filter"),
        miniContentPanel(
          fillRow(
            flex = c(1,1,2),
            fillCol(
              flex = c(5,4),
              fillRow(
                flex = c(1,1),
                fillCol(
                  flex = c(1,1,1,1,1),
                  fillRow(
                    flex = c(2,1),
                    textInput("pkgname", "Package name:", value = ""),
                    actionButton("getpkg","Get")
                  ),
                  uiOutput("filtchapter"),
                  uiOutput("filtsection"),
                  uiOutput("filtsubsection"),
                  uiOutput("filttopic")
                ),
                fillCol(
                  flex = c(1,1,1,1,1),
                  uiOutput("filttype"),
                  uiOutput("filtlevel"),
                  uiOutput("filtbloom"),
                  uiOutput("filtdifficulty"),
                  uiOutput("filtkeywords")
                )
              ),
              dataTableOutput("filtered_questions")
            ),
            fillCol(
              flex = c(1,6),
              fillRow(
                flex = c(1,1),
                uiOutput("select_display"),
                actionButton("addquest", "Add question")
              ),
              uiOutput("lookexample")
            )
          )
        ),
        shinyBS::bsTooltip(
          id = "pkgname",
          title = "From which package the next question should be taken?",
          placement = "top", trigger = "hover"
        )
      ),
      
      
      ##########################################################################
      
      miniTabPanel(
        "Edit",
        icon = icon("edit"),
        miniContentPanel(
          fillCol(
            flex = c(1,1,10),
            fillRow(
              flex = c(1,1,1,1,1),
              textOutput("total_points"),
              textOutput("unique_questions"),
              textOutput("check_blocs"),
              actionButton("update_selection", "Update"),
              actionButton("validate_selection", "Validate")
            ),
            tags$hr(),
            rhandsontable::rHandsontableOutput("edit_preselection")
          )
        )
      ),
      
      ##########################################################################
      
      miniTabPanel(
        "Check",
        icon = icon("eye"),
        miniContentPanel(
          uiOutput("check_test")
        )
      ),
      
      ##########################################################################
      
      miniTabPanel(
        "Balance",
        icon = icon("balance-scale"),
        miniContentPanel(
          fillCol(
            flex = c(5,1,5),
            fillRow(
              flex= c(1,1,1,1),
              plotOutput("balchapt"),
              plotOutput("balsecpt"),
              plotOutput("balsubpt"),
              plotOutput("baltoppt")
            ),
            tags$hr(),
            fillRow(
              flex= c(1,1,1,1),
              plotOutput("baltyppt"),
              plotOutput("ballevpt"),
              plotOutput("balblopt"),
              plotOutput("baldifpt")
            )
          )
        )
      )
    )
  )
  

  server <- function(input, output, session) {
    
    # Bind variables for dplyr
    
    
    
    # Create reactive values
    tables <- reactiveValues()
    template <- data.frame(
      question_id = "tmp",
      question_nbr = "tmp",
      question_language = "tmp",
      extype = "tmp",
      exname = "tmp",
      showexname = TRUE,
      points = 0,
      show_points = TRUE,
      difficulty = "tmp",
      show_difficulty = TRUE,
      seed = 0,
      alternatives = 0,
      type_table = "tmp",
      currency = "tmp",
      test_or_solution = "tmp",
      stringsAsFactors = FALSE
    )
    tables$preselection <- template
    tables$clean_preselection <- template
    tables$test <- template
    tables$pkgname <- ""
    tables$in_all_languages <- c()
    
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
          pkgname = "tmp",
          question_id = "tmp",
          question_nbr = "tmp",
          question_language = "tmp",
          extype = "tmp",
          exname = "tmp",
          showexname = TRUE,
          points = 0,
          show_points = TRUE,
          difficulty = "tmp",
          show_difficulty = TRUE,
          seed = 0,
          alternatives = 0,
          type_table = "tmp",
          currency = "tmp",
          test_or_solution = "tmp",
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
    observeEvent(input$getpkg, {
      tables$pkgname <- input$pkgname
    })
    
    questionlist <- reactive({
      if (tables$pkgname != ""){
        questions <- eval(
          parse(text = paste0(input$pkgname,"::","str_questions"))
        )
        
        # Select questions available in appropriate languages
        in_all_languages <- questions %>%
          dplyr::select(question_nbr, question_language) %>%
          dplyr::filter(question_language %in% input$languages) %>%
          dplyr::group_by(question_nbr) %>%
          dplyr::count() %>%
          dplyr::filter(n == length(input$languages))
        
        questions <- questions %>%
          dplyr::filter(question_nbr %in% in_all_languages$question_nbr)
        
        tables$in_all_languages <- unique(in_all_languages$question_nbr)
        
        # Selection of questions which are neither included nor excluded
        questions <- questions %>%
          dplyr::filter(!(question_id %in% tables$exclusion$question_id)) %>%
          dplyr::filter(!(question_id %in% tables$preselection$question_id))
        
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
    
    
    ############################################################################
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
            "topic_code"
            )
          ) %>%
        dplyr::select(
          question_id, question_nbr,
          topic_id, topic_code,
          chapter_label, section_label,
          subsection_label, topic_label,
          field_id, objective, description,
          type, level, bloom, difficulty
        ) %>%
        dplyr::arrange(topic_code)
    })
    
    
    ############################################################################
    # Create filters
    # Chapter
    output$filtchapter <- renderUI({
      if (tables$pkgname != ""){
        make_filter(
          dataset = base_filters(), variable = "chapter_label",
          id = "slctchapter", label = "Chapter:"
        )
      }
    })
      
    afterfiltchapter <- reactive({
      if (tables$pkgname != ""){
        filter_data(
          dataset = base_filters(), variable = "chapter_label",
          filt = input$slctchapter, type = "selection"
        )
      }
    })
    
    # Section
    output$filtsection <- renderUI({
      if (tables$pkgname != ""){
        make_filter(
          dataset = afterfiltchapter(), variable = "section_label",
          id = "slctsection", label = "Section:"
        )
      }
    })
    afterfiltsection <- reactive({
      if (tables$pkgname != ""){
        filter_data(
          dataset = afterfiltchapter(), variable = "section_label",
          filt = input$slctsection, type = "selection"
        )
      }
    })
    
    # Subsection
    output$filtsubsection <- renderUI({
      if (tables$pkgname != ""){
        make_filter(
          dataset = afterfiltsection(), variable = "subsection_label",
          id = "slctsubsection", label = "Sub-section:"
        )
      }
    })
    afterfiltsubsection <- reactive({
      if (tables$pkgname != ""){
        filter_data(
          dataset = afterfiltsection(), variable = "subsection_label",
          filt = input$slctsubsection, type = "selection"
        )
      }
    })
    
    # Topic
    output$filttopic <- renderUI({
      if (tables$pkgname != ""){
        make_filter(
          dataset = afterfiltsubsection(), variable = "topic_label",
          id = "slcttopic", label = "Topic:"
        )
      }
    })
    afterfilttopic <- reactive({
      if (tables$pkgname != ""){
        filter_data(
          dataset = afterfiltsubsection(), variable = "topic_label",
          filt = input$slcttopic, type = "selection"
        )
      }
    })
    
    # type
    output$filttype <- renderUI({
      if (tables$pkgname != ""){
        make_filter(
          dataset = afterfilttopic(), variable = "type",
          id = "slcttype", label = "Question type:"
        )
      }
    })
    afterfilttype <- reactive({
      if (tables$pkgname != ""){
        filter_data(
          dataset = afterfilttopic(), variable = "type",
          filt = input$slcttype, type = "selection"
        )
      }
    })
    
    # level
    output$filtlevel <- renderUI({
      if (tables$pkgname != ""){
        make_filter(
          dataset = afterfilttype(), variable = "level",
          id = "slctlevel", label = "Level:"
        )
      }
    })
    afterfiltlevel <- reactive({
      if (tables$pkgname != ""){
        filter_data(
          dataset = afterfilttype(), variable = "level",
          filt = input$slctlevel, type = "selection"
        )
      }
    })
    
    # bloom
    output$filtbloom <- renderUI({
      if (tables$pkgname != ""){
        make_filter(
          dataset = afterfiltlevel(), variable = "bloom",
          id = "slctbloom", label = "Bloom:"
        )
      }
    })
    afterfiltbloom <- reactive({
      if (tables$pkgname != ""){
        filter_data(
          dataset = afterfiltlevel(), variable = "bloom",
          filt = input$slctbloom, type = "selection"
        )
      }
    })
    
    # Difficulty
    output$filtdifficulty <- renderUI({
      if (tables$pkgname != ""){
        make_filter(
          dataset = afterfiltbloom(), variable = "difficulty",
          id = "slctdifficulty", label = "Difficulty:"
        )
      }
    })
    afterfiltdifficulty <- reactive({
      if (tables$pkgname != ""){
        filter_data(
          dataset = afterfiltbloom(), variable = "difficulty",
          filt = input$slctdifficulty, type = "selection"
        )
      }
    })
    
    # Keywords
    output$filtkeywords <- renderUI({
      if (tables$pkgname != ""){
        textInput(
          "slctkeywords",
          "Keywords:",
          value = "",
          width = "100%"
        )
      }
    })
    afterfiltkeywords <- reactive({
      if (tables$pkgname != ""){
        inobj <- afterfiltdifficulty() %>%
          filter_data(
            variable = "objective",
            filt = input$slctkeywords,
            type = "text"
          )
        
        afterfiltdifficulty() %>%
          dplyr::filter(question_nbr %in% inobj$question_nbr)
      }
    })
    
    ############################################################################
    # Interface to see filtered questions
    output$filtered_questions <- shiny::renderDataTable({
      if (tables$pkgname != ""){
        afterfiltkeywords() %>%
          dplyr::select(question_id, objective, description) %>%
          unique() %>%
          dplyr::arrange(question_id)
      }
    })
    
    output$select_display <- renderUI({
      if (tables$pkgname != "") {
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
      if (tables$pkgname != "" & !is.null(input$slctdisp)){
        htmldoc <- system.file(
          "examples",
          paste0(input$slctdisp, ".html"),
          package="mancon"
        )
        page <- xml2::read_html(htmldoc)
        withMathJax(HTML(as.character(rvest::html_node(page, "body"))))
      }
    })
    
    observeEvent(input$addquest, {
      selected_question <- questionlist() %>%
        filter(question_id == input$slctdisp)
      
      for (lang in input$languages){
        add <- data.frame(
          pkgname = tables$pkgname,
          question_id = selected_question$question_id[1],
          question_nbr = selected_question$question_nbr[1],
          question_language = input$languages[1],
          extype = dplyr::case_when(
            input$typeanswer == "choice" ~ "schoice",
            input$typeanswer == "number" ~ "num",
            TRUE ~ "string"
          ),
          exname = "tmp",
          showexname = input$showexname,
          points = 1,
          show_points = input$showquestpoint,
          difficulty = selected_question$difficulty[1],
          show_difficulty = input$showquestdifficulty[1],
          seed = 0,
          alternatives = input$alternatives,
          type_table = "html",
          currency = input$currency,
          test_or_solution = "test",
          stringsAsFactors = FALSE
        )
      }
      
      tables$preselection <- tables$preselection %>%
        dplyr::bind_rows(add)
      
      # Create versions
      base <- tables$preselection %>%
        dplyr::select(-question_id, -question_language, -exname, -seed) %>%
        unique() %>%
        dplyr::filter(question_nbr %in% tables$in_all_languages) %>%
        dplyr::mutate(question_language = input$languages[1]) %>%
        dplyr::mutate(question_id = paste0(question_nbr, question_language)) %>%
        dplyr::mutate(seed = 0) %>%
        tibble::rownames_to_column("exname")
      
      tmpbase <- list()
      
      for (version in seq_len(input$questversions)) {
        seed = 1000+floor(stats::runif(1)*8999)
        tmpbase[[version]] <- base %>%
          dplyr::mutate(
            exname = paste0(
              input$prefix,
              exname,
              "-V", version,
              input$languages[1]
            ),
            seed = seed
          )
      }
      
      base <- dplyr::bind_rows(tmpbase)
      
      tmpbase <- list()
      
      for (lang in input$languages) {
        tmpbase[[lang]] <- base %>%
          dplyr::mutate(
            question_id = stringr::str_replace_all(
              question_id,
              input$languages[1],
              lang
            ),
            question_language = lang,
            exname = stringr::str_replace_all(
              exname,
              input$languages[1],
              lang
            ),
          )
      }
      
      base <- dplyr::bind_rows(tmpbase) %>%
        dplyr::mutate(remove = FALSE) %>%
        dplyr::arrange(exname)
      
      base$seed <- 1000+floor(stats::runif(nrow(base))*8999)
      
      tables$clean_preselection <- base
    })
    
    
    ############################################################################
    # Prepare editable selection of questions
    
    
    output$edit_preselection <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        tables$clean_preselection,
        height = 400,
        width = "100%",
        stretchH = "all"
      ) %>%
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE,
          allowColEdit = FALSE
        )
    })

    output$total_points <- renderText({
      points <- tables$clean_preselection %>%
        dplyr::filter(
          stringr::str_detect(exname, paste0("V1", input$languages[1]))
        ) %>%
        dplyr::select(points) %>%
        unlist() %>% as.numeric() %>% sum()
      paste0(points, " points.")
    })
    
    output$unique_questions <- renderText({
      paste0(
        length(unique(tables$clean_preselection$question_nbr)),
        " unique questions."
      )
    })
    
    output$check_blocs <- renderText({
      actual <- length(unique(tables$clean_preselection$question_nbr))
      target <- input$blocnbr * input$blocsize
      delta <- target - actual
      if (target > 1) {
        showdelta <- dplyr::case_when(
          delta < 0 ~ paste0("You need ", abs(delta)," less question(s)."),
          delta > 0 ~ paste0("You need ", delta," more question(s)."),
          TRUE ~ "The number of questions is adequate."
        )
      } else showdelta <- ""
      showdelta
    })
    
    observeEvent(input$update_selection, {
      edited <- suppressWarnings(
        rhandsontable::hot_to_r(input$edit_preselection)
      )
      edited <- dplyr::filter(edited, remove == FALSE)
      tables$clean_preselection <- edited
    })
    
    observeEvent(input$validate_selection, {
      tables$test <- tables$clean_preselection
    })
    
    ############################################################################
    # Check exam
    base_test <- reactive({
      if (nrow(tables$test) > 1){
        base_test <- tables$test %>%
          dplyr::select(pkgname, question_nbr, exname, points) %>%
          dplyr::filter(
            stringr::str_detect(
              exname,
              paste0("V1", input$languages[1])
            )
          ) %>%
          unique() %>%
          dplyr::mutate(question_id = paste0(question_nbr, input$languages[1])) %>%
          dplyr::group_by(pkgname) %>%
          tidyr::nest() %>%
          dplyr::mutate(
            data = purrr::map2(data, pkgname, append_questions, lang = input$languages[1])
          ) %>%
          tidyr::unnest(data) %>%
          dplyr::ungroup()
        base_test
      }
    })
    
    
    output$check_test <- renderUI({
      if (nrow(tables$test) > 1){
        questions <- base_test()
        
        test <- c()
        for (i in seq_len(nrow(questions))){
          address <- system.file(
            "examples",
            paste0(questions$question_id[i], ".html"),
            package = questions$pkgname[i]
          )
          page <- xml2::read_html(address)
          test[questions$exname[i]] <- as.character(
            rvest::html_node(page, "body")
          )
        }
        withMathJax(HTML(test))
      }
    })
    
    
    ##############################
    
    output$balchapt <- renderPlot({
      base_test() %>%
        dplyr::group_by(chapter) %>%
        dplyr::summarise(points = sum(points)) %>%
        ggplot(aes(x = chapter, y = points)) +
        geom_bar(stat = "identity") +
        coord_flip()
    })
    
    output$balsecpt <- renderPlot({
      base_test() %>%
        dplyr::group_by(section) %>%
        dplyr::summarise(points = sum(points)) %>%
        ggplot(aes(x = section, y = points)) +
        geom_bar(stat = "identity") +
        coord_flip()
    })
    
    output$balsubpt <- renderPlot({
      base_test() %>%
        dplyr::group_by(subsection) %>%
        dplyr::summarise(points = sum(points)) %>%
        ggplot(aes(x = subsection, y = points)) +
        geom_bar(stat = "identity") +
        coord_flip()
    })
    
    output$baltoppt <- renderPlot({
      base_test() %>%
        dplyr::group_by(topic) %>%
        dplyr::summarise(points = sum(points)) %>%
        ggplot(aes(x = topic, y = points)) +
        geom_bar(stat = "identity") +
        coord_flip()
    })
    
    
    
    output$baltyppt <- renderPlot({
      base_test() %>%
        dplyr::group_by(type) %>%
        dplyr::summarise(points = sum(points)) %>%
        ggplot(aes(x = type, y = points)) +
        geom_bar(stat = "identity") +
        coord_flip()
    })
    
    output$ballevpt <- renderPlot({
      base_test() %>%
        dplyr::group_by(level) %>%
        dplyr::summarise(points = sum(points)) %>%
        ggplot(aes(x = level, y = points)) +
        geom_bar(stat = "identity") +
        coord_flip()
    })
    
    output$balblopt <- renderPlot({
      base_test() %>%
        dplyr::group_by(bloom) %>%
        dplyr::summarise(points = sum(points)) %>%
        ggplot(aes(x = bloom, y = points)) +
        geom_bar(stat = "identity") +
        coord_flip()
    })
    
    output$baldifpt <- renderPlot({
      base_test() %>%
        dplyr::group_by(difficulty) %>%
        dplyr::summarise(points = sum(points)) %>%
        ggplot(aes(x = difficulty, y = points)) +
        geom_bar(stat = "identity") +
        coord_flip()
    })
    

    
    
    
    
    
    
    
    
    
    
    #################
    # On exit

    observeEvent(input$done, {
      
      test_name <- paste0(input$name,"_",input$date)
      
      # Setup the test folder structure if it does not exist
      if (!dir.exists(test_name)) dir.create(test_name)
      if (!dir.exists(paste0(test_name, "/1_students")))
        dir.create(paste0(test_name, "/1_students"))
      if (!dir.exists(paste0(test_name, "/2_parameters")))
        dir.create(paste0(test_name, "/2_parameters"))
      if (!dir.exists(paste0(test_name, "/3_test")))
        dir.create(paste0(test_name, "/3_test"))
      if (input$blackboard_out &
          !dir.exists(paste0(test_name, "/3_test/blackboard")))
        dir.create(paste0(test_name, "/3_test/blackboard"))
      if (input$canvas_out &
          !dir.exists(paste0(test_name, "/3_test/canvas")))
        dir.create(paste0(test_name, "/3_test/canvas"))
      if (input$moodle_out &
          !dir.exists(paste0(test_name, "/3_test/moodle")))
        dir.create(paste0(test_name, "/3_test/moodle"))
      if (input$openolat_out &
          !dir.exists(paste0(test_name, "3/_test/openolat")))
        dir.create(paste0(test_name, "3/_test/openolat"))
      if (input$arsnova_out &
          !dir.exists(paste0(test_name, "3/_test/arsnova")))
        dir.create(paste0(test_name, "3/_test/arsnova"))
      if (input$tcexam_out &
          !dir.exists(paste0(test_name, "/3_test/tcexam")))
        dir.create(paste0(test_name, "/3_test/tcexam"))
      if (input$pdf_out &
          !dir.exists(paste0(test_name, "/3_test/pdf")))
        dir.create(paste0(test_name, "/3_test/pdf"))
      if (input$html_out &
          !dir.exists(paste0(test_name, "/3_test/html")))
        dir.create(paste0(test_name, "/3_test/html"))
      if (input$pandoc_out &
          !dir.exists(paste0(test_name, "3_test/pandoc")))
        dir.create(paste0(test_name, "3_test/pandoc"))
      if (input$nops_out &
          !dir.exists(paste0(test_name, "3_test/nops")))
        dir.create(paste0(test_name, "3_test/nops"))
      if (input$lops_out &
          !dir.exists(paste0(test_name, "/3_test/lops")))
        dir.create(paste0(test_name, "3_test/lops"))
      if (input$qui12_out &
          !dir.exists(paste0(test_name, "3_test/qui12")))
        dir.create(paste0(test_name, "/3_test/qui12"))
      if (!dir.exists(paste0(test_name, "/4_answers")))
        dir.create(paste0(test_name, "/4_answers"))
      if (!dir.exists(paste0(test_name, "/5_matrix")))
        dir.create(paste0(test_name, "/5_matrix"))
      if (!dir.exists(paste0(test_name, "/5_matrix/raw")))
        dir.create(paste0(test_name, "/5_matrix/calibrated"))
      if (!dir.exists(paste0(test_name, "/6_reports")))
        dir.create(paste0(test_name, "/6_reports"))
      if (!dir.exists(paste0(test_name, "/rmd")))
        dir.create(paste0(test_name, "/rmd"))
      if (!dir.exists(paste0(test_name, "/tmp")))
        dir.create(paste0(test_name, "/tmp"))
      
      
      # Retrieve the list of questions to generate and retrieve templates
      parameters <- tables$test %>%
        dplyr::group_by(question_language) %>%
        tidyr::nest()
      
      purrr::pmap(
        parameters,
        function(question_language, data)
          write.csv(
            data,
            file = paste0(
              test_name,
              "/2_parameters/",
              question_language,
              ".csv"
            )
          )
      )
      
      
      
      

      stopApp()
    })
  }
  
  runGadget(
    ui,
    server,
    viewer = shiny::dialogViewer("genTest", 1920, 1080)
  )
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


# Function to append str_questions to base_test
append_questions <- function(x, pkgname, lang){
  questions <- eval(
    parse(text = paste0(pkgname,"::","str_questions"))
  )
  labels <- eval(
      parse(text = paste0(pkgname,"::","str_labels"))
    )%>%
    dplyr::filter(language == lang) %>%
    dplyr::select(
      topic_id,
      chapter = chapter_label,
      section = section_label,
      subsection = subsection_label,
      topic = topic_label
    )
  
  x %>%
    dplyr::select(-question_nbr) %>%
    dplyr::left_join(questions, by = c("question_id")) %>%
    dplyr::left_join(labels, by = c("topic_id")) %>%
    dplyr::mutate(
      chapter = paste(substr(topic_code, 3,3), chapter, sep = "-"),
      section = paste(substr(topic_code, 3,4), section, sep = "-"),
      subsection = paste(substr(topic_code, 3,5), subsection, sep = "-"),
      topic = paste(substr(topic_code, 3,6), topic, sep = "-")
    ) %>%
    dplyr::select(
      exname, question_id, points,
      type, level, bloom, difficulty,
      chapter, section, subsection, topic,
      points
    )
}
