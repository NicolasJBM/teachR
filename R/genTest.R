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
#' @importFrom shiny browserViewer
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
#' @importFrom dplyr ungroup
#' @importFrom dplyr n
#' @importFrom tidyr spread
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tibble rownames_to_column
#' @importFrom exams exams2canvas
#' @importFrom exams exams2openolat
#' @importFrom exams exams2arsnova
#' @importFrom exams exams2tcexam
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
#' @importFrom stringr str_extract
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
          flex = c(1, 1),
          fillCol(
            flex = c(1, 1, 1, 4),
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
              value = "TEST"
            ),
            textInput(
              inputId = "prefix",
              label = "Question prefix:",
              value = "Q"
            )
          ),

          fillCol(
            flex = c(1, 1, 1, 4),
            dateInput(
              inputId = "date",
              label = "Date:",
              value = Sys.Date()
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
          flex = c(1, 1, 1),

          fillCol(
            flex = c(1, 1, 1, 1, 1, 1, 1, 2),
            h4("Delivery format"),
            selectInput(
              "languages",
              "Select the language(s):",
              choices = c("DE", "EN", "ES", "FR", "IT", "NL"),
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
              choices = c("choice", "number", "text"),
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
            flex = c(1, 8),
            h4("Delivery platforms"),
            fillCol(
              flex = c(1, 1, 1, 1, 1, 1, 1, 1),
              checkboxInput("blackboard_out", "Blackboard", value = TRUE),
              checkboxInput("canvas_out", "Canvas", value = FALSE),
              checkboxInput("moodle_out", "Moodle", value = FALSE),
              checkboxInput("openolat_out", "Open OLAT", value = FALSE),
              checkboxInput("arsnova_out", "ARS Novaova", value = FALSE),
              checkboxInput("tcexam_out", "tcexam", value = FALSE),
              checkboxInput("html_out", "html", value = TRUE),
              checkboxInput("pdf_out", "PDF", value = FALSE)
            )
          ),

          fillCol(
            flex = c(1, 1, 1, 5),
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
              numericInput(
                inputId = "versionnbr",
                label = "Number of versions",
                value = 1,
                min = 1,
                max = 10,
                step = 1
              ),
              conditionalPanel(
                condition = "input.versionnbr > 1",
                numericInput(
                  inputId = "blocnbr",
                  label = "Number of blocs",
                  value = 2,
                  min = 2,
                  max = 10,
                  step = 1
                ),
                numericInput(
                  inputId = "blocsize",
                  label = "Number of questions per bloc",
                  value = 2,
                  min = 2,
                  max = 45,
                  step = 1
                )
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
            flex = c(1, 1, 2),
            fillCol(
              flex = c(5, 4),
              fillRow(
                flex = c(1, 1),
                fillCol(
                  flex = c(1, 1, 1, 1, 1),
                  fillRow(
                    flex = c(2, 1),
                    textInput("pkgname", "Package name:", value = ""),
                    actionButton("getpkg", "Get")
                  ),
                  uiOutput("filtchapter"),
                  uiOutput("filtsection"),
                  uiOutput("filtsubsection"),
                  uiOutput("filttopic")
                ),
                fillCol(
                  flex = c(1, 1, 1, 1, 1),
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
              flex = c(1, 6),
              fillRow(
                flex = c(1, 1),
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
            flex = c(1, 1, 10),
            fillRow(
              flex = c(1, 1, 1, 1),
              textOutput("total_points"),
              textOutput("unique_questions"),
              textOutput("check_blocs"),
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
            fillRow(
              flex = c(1, 1, 1, 1),
              plotOutput("balchapt"),
              plotOutput("balsecpt"),
              plotOutput("balsubpt"),
              plotOutput("baltoppt")
            ),
            tags$hr(),
            fillRow(
              flex = c(1, 1, 1, 1),
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
    bloom <- NULL
    chapter <- NULL
    chapter_label <- NULL
    description <- NULL
    difficulty <- NULL
    stage <- NULL
    exname <- NULL
    field_id <- NULL
    language <- NULL
    level <- NULL
    objective <- NULL
    pkgname <- NULL
    points <- NULL
    question_id <- NULL
    question_language <- NULL
    question_nbr <- NULL
    section <- NULL
    section_label <- NULL
    subsection <- NULL
    subsection_label <- NULL
    topic <- NULL
    topic_code <- NULL
    topic_id <- NULL
    topic_label <- NULL
    type <- NULL
    criterion_id <- NULL
    criterion_label <- NULL
    criterion_language <- NULL
    criterion_nbr <- NULL
    criterion_order <- NULL
    V1 <- NULL
    data <- NULL
    pkg <- NULL

    # Create reactive values
    tables <- reactiveValues()
    tables$pkgname <- ""
    tables$in_all_languages <- c()

    # Retrieve uploaded files
    observe({
      if (is.null(input$studentlist)) {
        tables$students_list <- data.frame(
          student_id = "s9999999",
          first_name = "John",
          last_name = "Doe",
          e_mail = "john_doe@noone.com",
          language = "EN",
          stringsAsFactors = FALSE
        )
      } else {
        tables$students_list <- as.data.frame(
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
        template <- data.frame(
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
          stringsAsFactors = FALSE
        )
        tables$preselection <- template
        tables$clean_preselection <- template
        tables$test <- template
      } else {
        template <- utils::read.csv(
          file = input$preselection$datapath[[1]],
          stringsAsFactors = FALSE
        )
        tables$preselection <- template
        tables$clean_preselection <- template
        tables$test <- template
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
      questions <- teachR::get_pkg_data(tables$pkgname, "str_questions") %>%
        dplyr::filter(stage == "public")
      in_all_languages <- questions %>%
        dplyr::select(question_nbr, question_language) %>%
        dplyr::filter(question_language %in% input$languages) %>%
        dplyr::group_by(question_nbr) %>%
        dplyr::count() %>%
        dplyr::filter(n == length(input$languages))
      tables$in_all_languages <- c(
        tables$in_all_languages,
        unique(in_all_languages$question_nbr)
      )
    })

    questionlist <- reactive({
      if (tables$pkgname != "") {

        # Select published questions (exclude "design" and "review" stages)
        questions <- teachR::get_pkg_data(tables$pkgname, "str_questions") %>%
          dplyr::filter(stage == "public")
        
        # Select questions available in appropriate languages
        questions <- questions %>%
          dplyr::filter(question_nbr %in% tables$in_all_languages)
        
        # Selection of questions which are not excluded
        questions <- questions %>%
          dplyr::filter(!(question_id %in% tables$exclusion$question_id))
        
        # Selection of questions in appropriate format
        if (input$typeanswer == "choice") {
          questions <- dplyr::filter(
            questions, type %in% c(
              "1 Statements", "2 Alternatives", "3 Assessement", "4 Computation"
            )
          )
        }
        
        if (input$typeanswer == "number") {
          questions <- dplyr::filter(
            questions, type %in% c("4 Computation")
          )
        }
        
        if (input$typeanswer == "text") {
          questions <- dplyr::filter(
            questions, type %in% c(
              "4 Computation", "5 Question",
              "6 Problem", "7 Essay", "8 Case"
            )
          )
        }
      }
      questions
    })


    ############################################################################
    # Prepare filters
    base_filters <- reactive({
      filters <- teachR::get_pkg_data(tables$pkgname, "str_labels")

      # Selection of questions which are not excluded
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
        dplyr::mutate(
          chapter_label = paste0(
            substr(topic_code, 3, 3),
            " ", chapter_label
          ),
          section_label = paste0(
            substr(topic_code, 3, 4),
            " ", section_label
          ),
          subsection_label = paste0(
            substr(topic_code, 3, 5),
            " ", section_label
          ),
          topic_label = paste0(
            substr(topic_code, 3, 6),
            " ", section_label
          )
        )
    })


    ############################################################################
    # Create filters
    # Chapter
    output$filtchapter <- renderUI({
      if (tables$pkgname != "") {
        make_filter(
          dataset = base_filters(), variable = "chapter_label",
          id = "slctchapter", label = "Chapter:"
        )
      }
    })

    afterfiltchapter <- reactive({
      if (tables$pkgname != "") {
        filter_data(
          dataset = base_filters(), variable = "chapter_label",
          filt = input$slctchapter, type = "selection"
        )
      }
    })

    # Section
    output$filtsection <- renderUI({
      if (tables$pkgname != "") {
        make_filter(
          dataset = afterfiltchapter(), variable = "section_label",
          id = "slctsection", label = "Section:"
        )
      }
    })
    afterfiltsection <- reactive({
      if (tables$pkgname != "") {
        filter_data(
          dataset = afterfiltchapter(), variable = "section_label",
          filt = input$slctsection, type = "selection"
        )
      }
    })

    # Subsection
    output$filtsubsection <- renderUI({
      if (tables$pkgname != "") {
        make_filter(
          dataset = afterfiltsection(), variable = "subsection_label",
          id = "slctsubsection", label = "Sub-section:"
        )
      }
    })
    afterfiltsubsection <- reactive({
      if (tables$pkgname != "") {
        filter_data(
          dataset = afterfiltsection(), variable = "subsection_label",
          filt = input$slctsubsection, type = "selection"
        )
      }
    })

    # Topic
    output$filttopic <- renderUI({
      if (tables$pkgname != "") {
        make_filter(
          dataset = afterfiltsubsection(), variable = "topic_label",
          id = "slcttopic", label = "Topic:"
        )
      }
    })
    afterfilttopic <- reactive({
      if (tables$pkgname != "") {
        filter_data(
          dataset = afterfiltsubsection(), variable = "topic_label",
          filt = input$slcttopic, type = "selection"
        )
      }
    })

    # Type
    output$filttype <- renderUI({
      if (tables$pkgname != "") {
        make_filter(
          dataset = afterfilttopic(), variable = "type",
          id = "slcttype", label = "Question type:"
        )
      }
    })
    afterfilttype <- reactive({
      if (tables$pkgname != "") {
        filter_data(
          dataset = afterfilttopic(), variable = "type",
          filt = input$slcttype, type = "selection"
        )
      }
    })

    # Level
    output$filtlevel <- renderUI({
      if (tables$pkgname != "") {
        make_filter(
          dataset = afterfilttype(), variable = "level",
          id = "slctlevel", label = "Level:"
        )
      }
    })
    afterfiltlevel <- reactive({
      if (tables$pkgname != "") {
        filter_data(
          dataset = afterfilttype(), variable = "level",
          filt = input$slctlevel, type = "selection"
        )
      }
    })

    # Bloom
    output$filtbloom <- renderUI({
      if (tables$pkgname != "") {
        make_filter(
          dataset = afterfiltlevel(), variable = "bloom",
          id = "slctbloom", label = "Bloom:"
        )
      }
    })
    afterfiltbloom <- reactive({
      if (tables$pkgname != "") {
        filter_data(
          dataset = afterfiltlevel(), variable = "bloom",
          filt = input$slctbloom, type = "selection"
        )
      }
    })

    # Difficulty
    output$filtdifficulty <- renderUI({
      if (tables$pkgname != "") {
        make_filter(
          dataset = afterfiltbloom(), variable = "difficulty",
          id = "slctdifficulty", label = "Difficulty:"
        )
      }
    })
    afterfiltdifficulty <- reactive({
      if (tables$pkgname != "") {
        filter_data(
          dataset = afterfiltbloom(), variable = "difficulty",
          filt = input$slctdifficulty, type = "selection"
        )
      }
    })

    # Keywords
    output$filtkeywords <- renderUI({
      if (tables$pkgname != "") {
        textInput(
          "slctkeywords",
          "Keywords:",
          value = "",
          width = "100%"
        )
      }
    })
    afterfiltkeywords <- reactive({
      if (tables$pkgname != "") {
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
    # (after removing the questions already selected)
    output$filtered_questions <- shiny::renderDataTable({
      if (tables$pkgname != "") {
        preselected <- c("", unique(tables$preselection$question_id))
        filtered <- afterfiltkeywords() %>%
          dplyr::select(question_id, objective, description) %>%
          dplyr::filter(!(question_id %in% preselected)) %>%
          unique() %>%
          dplyr::arrange(question_id)
      }
    })

    output$select_display <- renderUI({
      if (tables$pkgname != "") {
        preselected <- c("", unique(tables$preselection$question_id))
        choices <- afterfiltkeywords() %>%
          dplyr::filter(!(question_id %in% preselected)) %>%
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
      if (tables$pkgname != "" & !is.null(input$slctdisp)) {
        if (input$slctdisp != "") {
          htmldoc <- system.file(
            "examples",
            paste0(input$slctdisp, ".html"),
            package = tables$pkgname
          )
          if (file.exists(htmldoc)){
            page <- xml2::read_html(htmldoc)
            withMathJax(HTML(as.character(rvest::html_node(page, "body"))))
          }
        }
      }
    })

    observeEvent(input$addquest, {
      selected_question <- questionlist() %>%
        filter(question_id == input$slctdisp)

      # Add the question in every selected language
      for (lang in input$languages) {
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
          currency = input$currency,
          stringsAsFactors = FALSE
        )
      }

      # Update the preselection
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
        seed <- 1000 + floor(stats::runif(1) * 8999)
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

      base$seed <- 1000 + floor(stats::runif(nrow(base)) * 8999)

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
        unlist() %>%
        as.numeric() %>%
        sum()
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
      if (input$versionnbr > 1) {
        target <- input$blocnbr * input$blocsize
      } else {
        target <- 1
      }
      delta <- target - actual
      if (target > 1) {
        showdelta <- dplyr::case_when(
          delta < 0 ~ paste0("You need ", abs(delta), " less question(s)."),
          delta > 0 ~ paste0("You need ", delta, " more question(s)."),
          TRUE ~ "The number of questions is adequate."
        )
      } else {
        showdelta <- ""
      }
      showdelta
    })

    observeEvent(input$validate_selection, {
      edited <- suppressWarnings(
        rhandsontable::hot_to_r(input$edit_preselection)
      )
      edited <- dplyr::filter(edited, remove == FALSE)
      tables$clean_preselection <- edited
      tables$test <- tables$clean_preselection %>%
        dplyr::arrange(exname)
    })

    ############################################################################
    # Check exam
    base_test <- reactive({
      basetest <- dplyr::filter(tables$test, exname != "tmp")
      if (nrow(basetest) > 0) {
        base_test <- basetest %>%
          dplyr::select(pkgname, question_nbr, exname, points) %>%
          dplyr::filter(
            stringr::str_detect(
              exname,
              paste0("V1", input$languages[1])
            )
          ) %>%
          unique() %>%
          dplyr::mutate(
            question_id = paste0(question_nbr, input$languages[1])
          ) %>%
          dplyr::group_by(pkgname) %>%
          tidyr::nest() %>%
          dplyr::mutate(
            data = purrr::map2(
              data,
              pkgname,
              append_questions,
              lang = input$languages[1]
            )
          ) %>%
          tidyr::unnest(data) %>%
          dplyr::ungroup()
        base_test
      } else {
        basetest
      }
    })

    output$check_test <- renderUI({
      if (nrow(base_test()) > 0) {
        questions <- base_test()

        test <- c()
        for (i in seq_len(nrow(questions))) {
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
      incr <- 0.05

      withProgress(message = "Generate tests", {

        # Retrieve questions from all listed packages
        all_pkg_questions <- tibble::tibble(
          pkg = unique(tables$test$pkgname)
        ) %>%
          dplyr::mutate(
            questions = purrr::map(
              pkg,
              get_pkg_data,
              "str_questions"
            )
          ) %>%
          tidyr::unnest(questions)

        # Setup the test folder structure if it does not exist
        incProgress(amount = incr, detail = "Create folders")
        test_name <- paste0(input$name, "_", input$date)
        if (!dir.exists(test_name)) dir.create(test_name)
        create_folder(test_name, "/1_parameters")
        create_folder(test_name, "/2_test")
        create_folder(test_name, "/2_test/rmd")
        create_folder(test_name, "/2_test/tmp")
        create_folder(test_name, "/3_answers")
        create_folder(test_name, "/3_answers/raw")
        create_folder(test_name, "/3_answers/structure")
        create_folder(test_name, "/4_matrix")
        create_folder(test_name, "/4_matrix/raw")
        create_folder(test_name, "/4_matrix/calibrated")
        create_folder(test_name, "/5_reports")

        # Store parameters per language
        incProgress(amount = incr, detail = "Create parameters")
        write.csv(
          tables$test,
          paste0(test_name, "/1_parameters/parameters.csv"),
          row.names = FALSE
        )

        # Retrieve .Rmd files
        incProgress(amount = incr, detail = "Retrieve questions")
        files <- tables$test %>%
          dplyr::select(question_id, exname, pkgname) %>%
          unique() %>%
          dplyr::mutate(test_name = test_name)

        purrr::pmap(files, retrieve_file)

        # Define paths to relevant directories
        incProgress(amount = incr, detail = "Define paths")
        wd <- paste0(getwd(), "/", test_name)
        pardir <- paste0(wd, "/1_parameters")
        tstdir <- paste0(wd, "/2_test")
        rmddir <- paste0(tstdir, "/rmd")
        tmpdir <- paste0(tstdir, "/tmp")

        # Generate the exams on each platform for each language
        for (lang in input$languages) {

          # Save student list
          incProgress(amount = incr, detail = paste0(lang, ": students list"))
          students <- tables$students_list %>%
            dplyr::filter(language == lang)
          write.csv(students, file = paste0(
            wd,
            "/3_answers/structure/",
            lang,
            "_students.csv"
          ), row.names = FALSE)

          # Save question list
          incProgress(amount = incr, detail = paste0(lang, ": questions list"))
          labels <- teachR::get_pkg_data(tables$pkgname, "str_labels") %>%
            dplyr::filter(language == lang) %>%
            dplyr::select(topic_code, topic_label)
          test <- tables$test %>%
            dplyr::filter(question_language == lang) %>%
            dplyr::select(question_id, exname)
          questions <- all_pkg_questions %>%
            dplyr::filter(
              question_id %in% unique(tables$test$question_id),
              question_language == lang
            ) %>%
            dplyr::select(
              question_id, question_nbr, topic_code, objective,
              type, level, bloom, difficulty
            ) %>%
            dplyr::left_join(labels, by = "topic_code") %>%
            dplyr::left_join(test, by = "question_id")
          write.csv(questions, file = paste0(
            wd,
            "/3_answers/structure/",
            lang,
            "_questions.csv"
          ), row.names = FALSE)

          # Save criteria for open questions
          incProgress(amount = incr, detail = paste0(lang, ": criteria list"))
          if (input$typeanswer == "text") {
            criteria <- teachR::get_pkg_data(
              input$pkgname,
              "str_open_criteria"
            ) %>%
              dplyr::filter(
                question_id %in% tables$test$question_id,
                criterion_language == lang
              ) %>%
              dplyr::select(
                question_id,
                criterion_id,
                criterion_nbr,
                criterion_order,
                criterion_label
              )
            write.csv(criteria, file = paste0(
              wd,
              "/3_answers/structure/",
              lang,
              "_criteria.csv"
            ), row.names = FALSE)
          }

          # Prepare list of questions
          incProgress(
            amount = incr,
            detail = paste0(lang, ": update parameters")
          )
          test_parameters <- read.csv(paste0(
            pardir, "/parameters.csv"
          )) %>%
            dplyr::filter(question_language == lang) %>%
            dplyr::mutate(files = paste0(exname, ".Rmd"))

          # Save parameters
          incProgress(amount = incr, detail = paste0(lang, ": save parameters"))
          save(
            test_parameters,
            file = paste0(
              rmddir, "/test_parameters.RData"
            )
          )
          
          # Initialize specifications
          test_or_solution <- "test"
          type_table <- "html"
          save(
            test_or_solution, type_table,
            file = paste0(rmddir, "/specifications.RData")
          )

          ######################################################################
          # Blackboard
          incProgress(
            amount = incr,
            detail = paste0(lang, ": generate for Blackboard")
          )
          if (input$blackboard_out) {
            outdir <- paste0(tstdir, "/blackboard")
            if (!dir.exists(outdir)) dir.create(outdir)
            exams::exams2blackboard(
              file = unlist(test_parameters$files),
              n = 1,
              name = paste0(test_name, "_blackboard_", lang),
              dir = outdir,
              edir = rmddir,
              tdir = tmpdir,
              points = test_parameters$points,
              quiet = TRUE,
              verbose = FALSE,
              zip = TRUE
            )
          }

          # Canvas
          incProgress(
            amount = incr,
            detail = paste0(lang, ": generate for Canvas")
          )
          if (input$canvas_out) {
            outdir <- paste0(tstdir, "/canvas")
            if (!dir.exists(outdir)) dir.create(outdir)
            exams::exams2canvas(
              file = unlist(test_parameters$files),
              n = 1,
              name = paste0(test_name, "_canvas_", lang),
              dir = outdir,
              edir = rmddir,
              tdir = tmpdir,
              points = test_parameters$points,
              quiet = TRUE,
              verbose = FALSE,
              zip = TRUE
            )
          }

          # Moodle
          incProgress(
            amount = incr,
            detail = paste0(lang, ": generate for Moodle")
          )
          if (input$moodle_out) {
            outdir <- paste0(tstdir, "/moodle")
            if (!dir.exists(outdir)) dir.create(outdir)
            exams::exams2moodle(
              file = unlist(test_parameters$files),
              n = 1,
              name = paste0(test_name, "_moodle_", lang),
              dir = outdir,
              edir = rmddir,
              tdir = tmpdir,
              points = test_parameters$points,
              quiet = TRUE,
              verbose = FALSE,
              zip = TRUE
            )
          }

          # OpenOlat
          incProgress(
            amount = incr,
            detail = paste0(lang, ": generate for Open Olat")
          )
          if (input$openolat_out) {
            outdir <- paste0(tstdir, "/openolat")
            if (!dir.exists(outdir)) dir.create(outdir)
            exams::exams2openolat(
              file = unlist(test_parameters$files),
              n = 1,
              name = paste0(test_name, "_openolat_", lang),
              dir = outdir,
              edir = rmddir,
              tdir = tmpdir,
              points = test_parameters$points,
              quiet = TRUE,
              verbose = FALSE,
              zip = TRUE
            )
          }

          # Arsnova
          incProgress(
            amount = incr,
            detail = paste0(lang, ": generate for ARS Nova")
          )
          if (input$arsnova_out) {
            outdir <- paste0(tstdir, "/arsnova")
            if (!dir.exists(outdir)) dir.create(outdir)
            exams::exams2arsnova(
              file = unlist(test_parameters$files),
              n = 1,
              name = paste0(test_name, "_arsnova_", lang),
              dir = outdir,
              edir = rmddir,
              tdir = tmpdir,
              points = test_parameters$points,
              quiet = TRUE,
              verbose = FALSE,
              zip = TRUE
            )
          }

          # Tcexam
          incProgress(
            amount = incr,
            detail = paste0(lang, ": generate for TCE Exam")
          )
          if (input$tcexam_out) {
            outdir <- paste0(tstdir, "/tcexam")
            if (!dir.exists(outdir)) dir.create(outdir)
            exams::exams2tcexam(
              file = unlist(test_parameters$files),
              n = 1,
              name = paste0(test_name, "_tcexam_", lang),
              dir = outdir,
              edir = rmddir,
              tdir = tmpdir,
              points = test_parameters$points,
              quiet = TRUE,
              verbose = FALSE,
              zip = TRUE
            )
          }

          # HTML
          incProgress(
            amount = incr,
            detail = paste0(lang, ": generate for HTML")
          )
          if (input$html_out) {
            outdir <- paste0(tstdir, "/html")
            if (!dir.exists(outdir)) dir.create(outdir)
            exams::exams2html(
              file = unlist(test_parameters$files),
              n = 1,
              name = paste0(test_name, "_html_", lang),
              dir = outdir,
              edir = rmddir,
              tdir = tmpdir,
              quiet = TRUE,
              verbose = FALSE
            )
          }


          ######################################################################

          # PDF
          incProgress(
            amount = incr,
            detail = paste0(lang, ": generate for print")
          )
          if (input$pdf_out) {
            outdir <- paste0(tstdir, "/pdf")
            if (!dir.exists(outdir)) dir.create(outdir)

            # Select one version of each question
            sample_versions <- test_parameters %>%
              dplyr::group_by(question_id) %>%
              dplyr::sample_n(1) %>%
              ungroup()

            # Cut of too many questions and assign to blocs
            nbr_quest <- input$blocnbr * input$blocsize
            nbr_quest <- min(nrow(sample_versions), nbr_quest)

            # Create different bloc ordering if requested
            if (input$versionnbr > 1) {
              order <- as.data.frame(
                matrix(seq_len(input$blocnbr), nrow = 1)
              )
              addorder <- as.data.frame(
                gtools::permutations(
                  input$blocnbr,
                  input$blocnbr,
                  repeats.allowed = FALSE
                )
              ) %>%
                dplyr::filter(V1 != 1) %>%
                dplyr::group_by(V1) %>%
                dplyr::sample_n(1) %>%
                dplyr::ungroup() %>%
                dplyr::sample_n((input$versionnbr - 1))

              order <- dplyr::bind_rows(order, addorder)
              names(order) <- gsub("V", "B", names(order))
              order$version <- paste0("V", seq_len(nrow(order)))
              write.csv(
                order,
                paste0(pardir, "/bloc_order.csv"),
                row.names = FALSE
              )
              order <- dplyr::select(order, -version)
            } else {
              order <- data.frame(B1 = 1)
            }

            for (i in seq_len(nrow(order))) {
              incProgress(
                amount = incr,
                detail = paste0(lang, ": generate version ", i)
              )

              version_id <- paste0("V", i)

              tmpversion <- sample_versions %>%
                dplyr::mutate(
                  bloc = dplyr::case_when(
                    input$versionnbr > 1 ~ paste0(
                      "B",
                      sort(rep(
                        seq_len(input$blocnbr),
                        input$blocsize
                      ))
                    ),
                    TRUE ~ "B1"
                  )
                )

              if (input$versionnbr > 1) {
                tmpversion <- split(tmpversion, tmpversion$bloc)
                tmpversion <- tmpversion[as.numeric(order[i, ])]
                tmpversion <- dplyr::bind_rows(tmpversion)
                write.csv(
                  tmpversion,
                  file = paste0(pardir, "/parameters_with_blocs.csv"),
                  row.names = FALSE
                )
              }

              version_name <- paste0(test_name, "_pdf_", version_id, "_", lang)

              template_test <- dplyr::case_when(
                input$typeanswer == "choice" ~
                paste0(
                  find.package("teachR"),
                  "/tex/",
                  lang,
                  "_MCQ_A4_TEST.tex"
                ),
                TRUE ~
                paste0(
                  find.package("teachR"),
                  "/tex/",
                  lang,
                  "_NUMTXT_A4_TEST.tex"
                ),
              )

              template_solution <- dplyr::case_when(
                input$typeanswer == "choice" ~
                paste0(
                  find.package("teachR"),
                  "/tex/",
                  lang,
                  "_MCQ_A4_SOL.tex"
                ),
                TRUE ~
                paste0(
                  find.package("teachR"),
                  "/tex/",
                  lang,
                  "_NUMTXT_A4_SOL.tex"
                ),
              )

              # Create the test
              test_or_solution <- "test"
              type_table <- "latex"
              save(
                test_or_solution, type_table,
                file = paste0(rmddir, "/specifications.RData")
              )
              test <- exams::exams2pdf(
                file = unlist(tmpversion$files),
                n = 1,
                name = paste0(version_name, "_questions"),
                dir = outdir,
                edir = rmddir,
                tdir = tmpdir,
                points = tmpversion$points,
                quiet = TRUE,
                verbose = FALSE,
                template = template_test
              )

              # Create the solution
              unlink(paste0(tmpdir, "/*"))
              test_or_solution <- "solution"
              type_table <- "latex"
              save(
                test_or_solution, type_table,
                file = paste0(rmddir, "/specifications.RData")
              )
              exams::exams2pdf(
                file = unlist(tmpversion$files),
                n = 1,
                name = paste0(version_name, "_solutions"),
                dir = outdir,
                edir = rmddir,
                tdir = tmpdir,
                points = tmpversion$points,
                quiet = TRUE,
                verbose = FALSE,
                template = template_solution
              )
            }
          }
        }
      })

      stopApp()
    })
  }

  runGadget(
    ui,
    server,
    viewer = shiny::browserViewer()
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
append_questions <- function(x, pkgname, lang) {
  bloom <- NULL
  chapter <- NULL
  chapter_label <- NULL
  difficulty <- NULL
  exname <- NULL
  language <- NULL
  level <- NULL
  points <- NULL
  question_id <- NULL
  question_nbr <- NULL
  section <- NULL
  section_label <- NULL
  subsection <- NULL
  subsection_label <- NULL
  topic <- NULL
  topic_code <- NULL
  topic_id <- NULL
  topic_label <- NULL
  type <- NULL

  questions <- teachR::get_pkg_data(pkgname, "str_questions")
  labels <- teachR::get_pkg_data(pkgname, "str_labels") %>%
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
      chapter = paste(substr(topic_code, 3, 3), chapter, sep = "-"),
      section = paste(substr(topic_code, 3, 4), section, sep = "-"),
      subsection = paste(substr(topic_code, 3, 5), subsection, sep = "-"),
      topic = paste(substr(topic_code, 3, 6), topic, sep = "-")
    ) %>%
    dplyr::select(
      exname, question_id, points,
      type, level, bloom, difficulty,
      chapter, section, subsection, topic,
      points
    )
}

# Function to create folders
create_folder <- function(test_name, folder) {
  if (!dir.exists(paste0(test_name, folder))) {
    dir.create(paste0(test_name, folder))
  }
}

# Function to retrieve Rmd files from the different packages
retrieve_file <- function(question_id, exname, pkgname, test_name) {
  destination <- paste0(test_name, "/", "2_test/rmd/", exname, ".Rmd")
  if (!file.exists(destination)) {
    source <- system.file(
      paste0("questions/", question_id, ".Rmd"),
      package = pkgname
    )
    file.copy(
      from = source,
      to = destination
    )
  }
}
