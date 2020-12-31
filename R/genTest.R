#' Shiny gadget to select test questions and generate both tests and solutions for several delivery formats.
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
#' @importFrom shiny uiOutput
#' @importFrom shiny actionButton
#' @importFrom shiny renderUI
#' @importFrom shiny renderText
#' @importFrom shiny renderTable
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
#' @importFrom DT renderDT
#' @importFrom DT DTOutput
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
#' @import writR
#' @import questR
#' @export


genTest <- function() {
  ui <- miniPage(
    theme = shinythemes::shinytheme("spacelab"),
    
    gadgetTitleBar("Test Generator"),
    
    miniTabstripPanel(
      
      #########################################################################
      
      miniTabPanel(
        "Information",
        icon = icon("edit"),
        fillCol(
          flex = c(1,1,1),
          
          fillRow(
            flex = c(1,1),
            textInput(
              inputId = "institution",
              label = "Institution:",
              value = ""
            ),
            textInput(
              inputId = "course",
              label = "Course:",
              value = ""
            )
          ),
          
          fillRow(
            textInput(
              flex = c(1,1),
              inputId = "name",
              label = "Name:",
              value = "test"
            ),
            dateInput(
              inputId = "date",
              label = "Date:",
              value = lubridate::today()
            )
          ),
          
          fileInput(
            "studentlist",
            "Student list:"
          )
          
        )
      ),
      
      #########################################################################
      
      miniTabPanel(
        "Delivery",
        icon = icon("paper-plane"),
        
        fillRow(
          flex = c(1,1,1,1),
          
          fillCol(
            flex = c(1,1,1,1,1,1),
            selectInput(
              inputId = "typeanswer",
              label = "Type of answer:",
              choices = c("choice","number","text"),
              selected = "multiple-choice"
            ),
            p("This will preselect questions compatible with this format."),
            tags$hr(),
            selectInput(
              inputId = "currency",
              label = "Currency used:",
              choices = c("euro", "dollar", "pound", "yen"),
              selected = "euro"
            ),
            checkboxInput(
              inputId = "showquestpt",
              label = "Show question points:",
              value = TRUE
            ),
            checkboxInput(
              inputId = "showquestcode",
              label = "Show question code:",
              value = TRUE
            )
          ),
          
          fillCol(
            flex = c(1,1,1,1,1,1,1,1,1,1,1,1,1),
            h3("Delivery platforms:"),
            checkboxInput("pdf_out", "PDF", value = TRUE),
            checkboxInput("blackboard_out", "Blackboard", value = TRUE),
            checkboxInput("canvas_out", "Canvas", value = FALSE),
            checkboxInput("moodle_out", "Moodle", value = FALSE),
            checkboxInput("pandoc_out", "Pandoc", value = FALSE),
            checkboxInput("nops_out", "nops", value = FALSE),
            checkboxInput("lops_out", "lops", value = FALSE),
            checkboxInput("html_out", "html", value = FALSE),
            checkboxInput("openolat_out", "openolat", value = FALSE),
            checkboxInput("arsnova_out", "arsnova", value = FALSE),
            checkboxInput("tcexam_out", "tcexam", value = FALSE),
            checkboxInput("qui12_out", "qti12", value = FALSE)
          ),
          
          fillCol(
            flex = c(1,1,1,1,1,1,1),
            h3("Delivery languages:"),
            checkboxInput("NL", "Ducth", value = FALSE),
            checkboxInput("EN", "English", value = TRUE),
            checkboxInput("FR", "French", value = FALSE),
            checkboxInput("DE", "German", value = FALSE),
            checkboxInput("IT", "Italian", value = FALSE),
            checkboxInput("ES", "Spanish", value = FALSE),
            p("This will preselect questions existing in all selected languages.")
          ),
          
          fillCol(
            flex = c(1,4,1),
            conditionalPanel(
              condition = "input.typeanswer == 'choice'",
              numericInput(
                inputId = "alternatives",
                label = "Number of alternatives",
                value = 4,
                min = 4,
                max = 5,
                step = 1
              )
            ),
            conditionalPanel(
              condition = "input.pdf_out == TRUE",
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
                inputId = "testversions",
                label = "Number of test versions",
                value = 1,
                min = 1,
                max = 10,
                step = 1
              ),
              numericInput(
                inputId = "blocsize",
                label = "Bloc size",
                value = 1,
                min = 1,
                max = 45,
                step = 1
              )
            ),
            numericInput(
              inputId = "questversions",
              label = "Number of question versions (for LMS)",
              value = 1,
              min = 1,
              max = 100,
              step = 1
            )
          )
        )
      ),
      
      #########################################################################
      
      
      
      
      
      
      
      
      
      miniTabPanel(
        "Selection",
        icon = icon("filter"),
        fillCol(
          flex = c(),
          miniContentPanel(
            fillRow(
              flex = c(1,1),
              fileInput(
                inputId = "selection",
                label = "Preselection of questions",
                accept = c(".csv"),
                multiple = FALSE
              ),
              fileInput(
                inputId = "exclusion",
                label = "Exclusion of questions",
                accept = c(".csv"),
                multiple = FALSE
              )
            ),
            
            
            
            
            fillRow(
              flex = c(1,1),
              fillCol(
                flex = c(7, 1, 2, 1),
                fillRow(
                  flex = c(1,2,1),
                  textInput("package", "Package", value = ""),
                  fillCol(
                    flex = c(1, 1, 1, 1, 1),
                    uiOutput(outputId = "filtchapter"),
                    uiOutput(outputId = "filtsection"),
                    uiOutput(outputId = "filtsubsection"),
                    uiOutput(outputId = "filtsubtopic"),
                    uiOutput(outputId = "filtobjective")
                  ),
                  fillCol(
                    flex = c(1, 1, 1, 1, 1),
                    uiOutput(outputId = "filttype"),
                    uiOutput(outputId = "filtlevel"),
                    uiOutput(outputId = "filtbloom"),
                    uiOutput(outputId = "filtdifficulty"),
                    textInput("filtkeyword", "Keywords", value = ""),
                    tags$br()
                  )
                ),
                tags$hr(),
                fillRow(
                  flex = c(1,1),
                  fillCol(
                    flex = c(1,1),
                    numericInput(
                      inputId = "points",
                      label = "Number of points",
                      min = 1,
                      max = 100,
                      value = 1,
                      step = 1,
                      width = '80%'
                    ),
                    actionButton(
                      inputId = "addSample",
                      label = "Add question",
                      width = "80%",
                      icon("plane")
                    )
                  ),
                  fillCol(
                    flex = c(1,1),
                    checkboxInput(
                      inputId = "allowduplicates",
                      label = "Allow duplicates",
                      value = FALSE
                    ),
                    textOutput("checkblocs")
                  )
                ),
                uiOutput("manslct")
              ),
              fillCol(
                flex = c(1,11),
                uiOutput("slctexample"),
                htmlOutput("lookexample")
              )
            )
          )
          
          
          
        )
      ),
      
      #########################################################################
      
      miniTabPanel(
        "Sort",
        icon = icon("sort-numeric-down"),
        
      ),
      
      #########################################################################
      
      miniTabPanel(
        "Check",
        icon = icon("eye"),
        miniContentPanel(
          uiOutput("viewquest")
        )
      ),
      
      #########################################################################
      
      miniTabPanel(
        "Balance",
        icon = icon("balance-scale"),
        miniContentPanel(
          fillCol(
            flex = c(1,1,10),
            fillRow(flex = c(1,1),
                    selectInput("tblrow", "Select rows",
                                choices = c("L1","L2","L3","L4"),
                                selected = "L1"),
                    selectInput("tblval",
                                "Select values",
                                choices = c("points","questions","difficulty"),
                                selected = "points")
                    
            ),
            tags$hr(),
            tableOutput("balance")
          )
        )
      )
    )
  )
  

  server <- function(input, output, session) {
    
    # Bind variables
    
    
    ################
    # Filter list and selection table creation
    
    tables <- reactiveValues()

    # Visibly bind variables (avoid notes in checks)
    
    
    observe({
      if (is.null(input$selection)) {
        tables$content <- data.frame(
          package = "tmp",
          questionid = "tmp",
          language = "tmp",
          type = "tmp",
          question_number = 0,
          question_version = 0,
          question_code = "tmp",
          seed = 1,
          points = 1,
          stringsAsFactors = FALSE
        )
      } else {
        tables$content <- as.data.frame(
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
          QN = "",
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
    
    tables$order <- c()
    
    questionlist <- reactive({
      questions <- questR::questions
      if (!is.null(input$language) & !is.null(input$typequest)){
        questions <- subset(questions, questions$language == input$language)
        questions <- subset(questions, questions$kind %in% c(input$typequest,"both"))
        questions <- subset(questions, !(questions$questionid %in% tables$exclusion$QN))
      }
      questions
    })

    
    
    ####################
    # Prepare filters
    output$filtsubject <- renderUI({
      choices <- sort(c(setdiff(unique(questionlist()$subject), ""), ""), decreasing = FALSE)
      selectInput("slctsubject",
                  "Subject:",
                  choices = choices,
                  selected = "",
                  multiple = FALSE,
                  width = '80%')
    })
    
    afterfiltsubject <- reactive({
      filter <- input$slctsubject
      if (is.null(filter)){
        questionlist()
      } else if (filter == "") {
        questionlist()
      } else {
        subset(questionlist(), stringr::str_detect(questionlist()$subject, filter))
      }
    })
    
    
    
    output$filtchapter <- renderUI({
      choices <- sort(c(setdiff(unique(afterfiltsubject()$chapter), ""), ""), decreasing = FALSE)
      selectInput("slctchapter",
                  "Chapter:",
                  choices = choices,
                  selected = "",
                  multiple = FALSE,
                  width = '80%')
    })
    
    afterfiltchapter <- reactive({
      filter <- input$slctchapter
      if (is.null(filter)){
        afterfiltsubject()
      } else if (filter == "") {
        afterfiltsubject()
      } else {
        subset(afterfiltsubject(), stringr::str_detect(afterfiltsubject()$chapter, filter))
      }
    })
    
    
    
    output$filtsection <- renderUI({
      choices <- sort(c(setdiff(unique(afterfiltchapter()$section), ""), ""), decreasing = FALSE)
      selectInput("slctsection",
                  "Section:",
                  choices = choices,
                  selected = "",
                  multiple = FALSE,
                  width = '80%')
    })
    
    afterfiltsection <- reactive({
      filter <- input$slctsection
      if (is.null(filter)){
        afterfiltchapter()
      } else if (filter == "") {
        afterfiltchapter()
      } else {
        subset(afterfiltchapter(), stringr::str_detect(afterfiltchapter()$section, filter))
      }
    })
    
    
    
    output$filtsubsection <- renderUI({
      choices <- sort(c(setdiff(unique(afterfiltsection()$subsection), ""), ""), decreasing = FALSE)
      selectInput("slctsubsection",
                  "Sub_section:",
                  choices = choices,
                  selected = "",
                  multiple = FALSE,
                  width = '80%')
    })
    
    afterfiltsubsection <- reactive({
      filter <- input$slctsubsection
      if (is.null(filter)){
        afterfiltsection()
      } else if (filter == "") {
        afterfiltsection()
      } else {
        subset(afterfiltsection(), stringr::str_detect(afterfiltsection()$subsection, filter))
      }
    })
    
    
    
    output$filtsubtopic <- renderUI({
      choices <- sort(c(setdiff(unique(afterfiltsubsection()$subtopic), ""), ""), decreasing = FALSE)
      selectInput("slctsubtopic",
                  "Subtopic:",
                  choices = choices,
                  selected = "",
                  multiple = FALSE,
                  width = '80%')
    })
    
    afterfiltsubtopic <- reactive({
      filter <- input$slctsubtopic
      if (is.null(filter)){
        afterfiltsubsection()
      } else if (filter == "") {
        afterfiltsubsection()
      } else {
        subset(afterfiltsubsection(), stringr::str_detect(afterfiltsubsection()$subtopic, filter))
      }
    })
    
    
    
    output$filtobjective <- renderUI({
      choices <- sort(c(setdiff(unique(afterfiltsubtopic()$objective), ""), ""), decreasing = FALSE)
      selectInput("slctobjective",
                  "Objective:",
                  choices = choices,
                  selected = "",
                  multiple = FALSE,
                  width = '80%')
    })
    
    afterfiltobjective <- reactive({
      filter <- input$slctobjective
      if (is.null(filter)){
        afterfiltsubtopic()
      } else if (filter == "") {
        afterfiltsubtopic()
      } else {
        subset(afterfiltsubtopic(), stringr::str_detect(afterfiltsubtopic()$objective, filter))
      }
    })
    
    
    
    output$filttype <- renderUI({
      choices <- sort(c(setdiff(unique(afterfiltobjective()$type), ""), ""), decreasing = FALSE)
      selectInput("slcttype",
                  "Type:",
                  choices = choices,
                  selected = "",
                  multiple = FALSE)
    })
    
    afterfilttype <- reactive({
      filter <- input$slcttype
      if (is.null(filter)){
        afterfiltobjective()
      } else if (filter == "") {
        afterfiltobjective()
      } else {
        subset(afterfiltobjective(), stringr::str_detect(afterfiltobjective()$type, filter))
      }
    })
    
    
    
    output$filtlevel <- renderUI({
      choices <- sort(c(setdiff(unique(afterfilttype()$level), ""), ""), decreasing = FALSE)
      selectInput("slctlevel",
                  "Level:",
                  choices = choices,
                  selected = "",
                  multiple = FALSE)
    })
    
    afterfiltlevel <- reactive({
      filter <- input$slctlevel
      if (is.null(filter)){
        afterfilttype()
      } else if (filter == "") {
        afterfilttype()
      } else {
        subset(afterfilttype(), stringr::str_detect(afterfilttype()$level, filter))
      }
    })
    
    
    
    output$filtbloom <- renderUI({
      choices <- sort(c(setdiff(unique(afterfiltlevel()$bloom), ""), ""), decreasing = FALSE)
      selectInput("slctbloom",
                  "Bloom:",
                  choices = choices,
                  selected = "",
                  multiple = FALSE)
    })
    
    afterfiltbloom <- reactive({
      filter <- input$slctbloom
      if (is.null(filter)){
        afterfiltlevel()
      } else if (filter == "") {
        afterfiltlevel()
      } else {
        subset(afterfiltlevel(), stringr::str_detect(afterfiltlevel()$bloom, filter))
      }
    })
    
    
    
    output$filtdifficulty <- renderUI({
      choices <- sort(c(setdiff(unique(afterfiltbloom()$difficulty), ""), ""), decreasing = FALSE)
      selectInput("slctdifficulty",
                  "Difficulty:",
                  choices = choices,
                  selected = "",
                  multiple = FALSE)
    })
    
    afterfiltdifficulty <- reactive({
      filter <- input$slctdifficulty
      if (is.null(filter)){
        afterfiltbloom()
      } else if (filter == "") {
        afterfiltbloom()
      } else {
        subset(afterfiltbloom(), stringr::str_detect(afterfiltbloom()$difficulty, filter))
      }
    })
    
    
    
    output$filtkeyword <- renderUI({
      textInput("slctkeyword",
                "Keywords:",
                value = "",
                width = '80%')
    })
    
    afterfiltkeyword <- reactive({
      filter <- input$slctkeyword
      if (is.null(filter)){
        afterfiltdifficulty()
      } else if (filter[[1]] == "") {
        afterfiltdifficulty()
      } else {
        keywords <- stringr::str_to_lower(unlist(str_split(filter, " ")))
        keywords <- stringr::str_replace_all(keywords, "_", " ")
        base <- afterfiltdifficulty()
        for (i in 1:length(keywords)) base <- subset(afterfiltdifficulty(), stringr::str_detect(stringr::str_to_lower(afterfiltdifficulty()$description), keywords[i]))
        base
      }
    })
    
    
    
    preselection <- reactive({
      reset <- input$addSample

      if (input$allowduplicates == FALSE) {
        available <- setdiff(afterfiltkeyword()$questionid, tables$contentexam$QN)
      } else {
        available <- afterfiltkeyword()$questionid
      }
      
      preselection <- subset(afterfiltkeyword(), afterfiltkeyword()$questionid %in% available)

      preselection
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
        manualselection <- input$manslct
        selectInput("slctexample",
                    "Select a question to see an example",
                    choices = manualselection,
                    selected = manualselection[1])
      }
    })
    
    output$lookexample <- renderUI({
      if (!is.null(input$slctexample)){
        address <- system.file("documents", paste0(input$slctexample, ".html"), package="questR")
        page <- xml2::read_html(address)
        withMathJax(HTML(as.character(rvest::html_node(page, "body"))))
      }
    })
    
    
    ##############################
    
    output$contentexam <- DT::renderDT({
      as.data.frame(tables$contentexam,
                    stringsAsFactors = FALSE)},
      options = list(pageLength = 10, lengthMenu = c(5, 10, 15), searching = FALSE, xtable.include.rownames = TRUE)
    )
    
    output$order <- renderUI({
      order <- tables$contentexam$ID
      selectInput("order", "Order", choices = order, selected = order, multiple = TRUE, width = "100%")
    })
    
    
    output$checkblocs <- renderText({
      if (!is.null(input$versions) & !is.null(input$blocsize)){
        versions <- input$versions
        blocsize <- input$blocsize
        selected <- nrow(tables$contentexam) 
        
        minquest <- versions * blocsize
        
        if (minquest > selected) missing <- minquest - selected else {
          modulus <- selected %% blocsize
          if (modulus > 0) missing <- blocsize - modulus else missing <- modulus
        }
        
        paste0("You need at least ", minquest, " questions or ", missing, " more question(s).")
      }
    })
    
    
    ##############################
    
    output$viewquest <- renderUI({
      if (nrow(tables$contentexam) > 1){
        questions <- tables$contentexam$QN
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
      
      if (input$tblval == "points"){
        balance <- tables$contentexam[,c(input$tblrow, "BL", "PT")]
        names(balance) <- c("Topic","Bloom","Value")
        balance <- balance %>%
          dplyr::group_by(Topic, Bloom) %>%
          dplyr::summarise(Value = sum(Value)) %>%
          dplyr::ungroup() %>%
          tidyr::spread(Bloom, Value, fill = 0) %>%
          writR::stat_totals(omit_col = "Topic", summary = "sum")
        
      } else if (input$tblval == "questions"){
        balance <- tables$contentexam[,c(input$tblrow, "BL")] %>%
          dplyr::mutate(Value = 1)
        names(balance) <- c("Topic","Bloom","Value")
        balance <- balance %>%
          dplyr::group_by(Topic, Bloom) %>%
          dplyr::summarise(Value = sum(Value)) %>%
          dplyr::ungroup() %>%
          tidyr::spread(Bloom, Value, fill = 0) %>%
          writR::stat_totals(omit_col = "Topic", summary = "sum")
        
      } else {
        balance <- tables$contentexam[,c(input$tblrow, "BL", "DI")] 
        names(balance) <- c("Topic","Bloom","Value")
        balance <- balance %>%
          dplyr::group_by(Topic, Bloom) %>%
          dplyr::summarise(Value = mean(Value)) %>%
          dplyr::ungroup() %>%
          tidyr::spread(Bloom, Value, fill = 0) %>%
          writR::stat_totals(omit_col = "Topic", summary = "mean")
      }
      balance
    })

    ##########################################
    # Actions to be executed on demand

    # Add the selected questions to the table
    observeEvent(input$addSample, {
      table <- tables$contentexam %>%
        dplyr::filter(ID != 0)

      if (!is.null(input$manslct)) {
        question <- sample(input$manslct, 1)
        questionNbr <- nrow(table) + 1

        add <- preselection() %>%
          dplyr::filter(questionid == question) %>%
          dplyr::mutate(
            ID = questionNbr,
            QN = question,
            PT = input$points,
            SD = sample(c(1:9999), 1)
          ) %>%
          dplyr::select(
            ID,
            QN,
            LG = language,
            SC = sectionid,
            LO = objective,
            TY = type,
            KD = kind,
            LV = level,
            BL = bloom,
            DI = difficulty,
            DE = description,
            SU = subject,
            L1 = chapter,
            L2 = section,
            L3 = subsection,
            L4 = subtopic,
            SD,
            PT
          )
        
        table <- table %>%
          dplyr::bind_rows(add) %>%
          na.omit() %>%
          dplyr::arrange(by = ID)
      }
      
      tables$contentexam <- table
    })

    
    observeEvent(input$reorder, {
      if (nrow(tables$contentexam) > 1 & length(input$order) > 0) {
        neworder <- as.integer(input$order)
        reordered <- tables$contentexam[neworder, ]
        reordered$ID <- c(1:nrow(reordered))
        tables$contentexam <- reordered
      }
      if (length(input$order) == 0) {
        tables$contentexam <- data.frame(
          ID = 0,
          QN = "tmp",
          LG = "tmp",
          SC = "tmp",
          LO = "tmp",
          TY = "tmp",
          KD = "tmp",
          LV = "tmp",
          BL = "tmp",
          DI = 0,
          DE = "tmp",
          SU = "tmp",
          L1 = "tmp",
          L2 = "tmp",
          L3 = "tmp",
          L4 = "tmp",
          SD = 0,
          PT = 0,
          stringsAsFactors = FALSE
        )
      }
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
          exasolu = "exam"
          save(exasolu, file = paste0(wd, "/parameters/exasolu.RData"))
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
          exasolu = "exam"
          save(exasolu, file = paste0(wd, "/parameters/exasolu.RData"))
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
          exasolu = "exam"
          save(exasolu, file = paste0(wd, "/parameters/exasolu.RData"))
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
            
            exasolu = "exam"
            save(exasolu, file = paste0(wd, "/parameters/exasolu.RData"))
            
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
            
            exasolu = "solution"
            save(exasolu, file = paste0(wd, "/parameters/exasolu.RData"))
            
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
