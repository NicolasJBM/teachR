#' Shiny gadget to select exam questions and generate both questions and solutions.
#' @return Save the different versions of exams and solutions.
#' @seealso gradexam()
#' @seealso checkexam()
#' @seealso sendexam()
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
#' @importFrom shiny textAreaInput
#' @importFrom shiny selectInput
#' @importFrom shiny checkboxInput
#' @importFrom shiny downloadButton
#' @importFrom shiny downloadHandler
#' @importFrom shiny stopApp
#' @importFrom shiny runGadget
#' @importFrom shiny conditionalPanel
#' @importFrom shiny tags
#' @importFrom shiny tableOutput
#' @importFrom shiny uiOutput
#' @importFrom shiny plotOutput
#' @importFrom shiny actionButton
#' @importFrom shiny renderUI
#' @importFrom shiny renderPlot
#' @importFrom shiny renderText
#' @importFrom shiny renderTable
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny withProgress
#' @importFrom shiny incProgress
#' @importFrom shiny includeHTML
#' @importFrom shiny withMathJax
#' @importFrom shiny dialogViewer
#' @importFrom shiny textOutput
#' @importFrom shiny htmlOutput
#' @importFrom shiny HTML
#' @importFrom shinythemes shinytheme
#' @importFrom dplyr %>%
#' @importFrom dplyr sample_n
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize_all
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom tidyr spread
#' @importFrom exams exams2nops
#' @importFrom exams exams2pdf
#' @importFrom exams exams2moodle
#' @importFrom exams exams2blackboard
#' @importFrom exams exams2html
#' @importFrom DT renderDT
#' @importFrom DT DTOutput
#' @importFrom tth tth
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom xml2 read_html
#' @importFrom rvest html_node
#' @importFrom gtools permutations
#' @importFrom stats na.omit
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom utils data
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#' @importFrom writR stat_totals
#' @export


genexam <- function() {
  ui <- miniPage(
    theme = shinythemes::shinytheme("spacelab"),
    
    gadgetTitleBar("Generator of Exams and Solutions"),
    miniTabstripPanel(
      miniTabPanel("Define",
        icon = icon("sliders"),
        miniContentPanel(
          fillCol(
            flex = c(1,1,1,1,3),
            
            fillRow(
              flex = c(1,1,1),
              textInput(
                inputId = "package_name",
                label = "Package of questions",
                value = "manacc"
              ),
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
              flex = c(1,1,1),
              selectInput(
                inputId = "language",
                label = "Language",
                choices = c("en"),
                selected = "en"
              ),
              selectInput(
                inputId = "currency",
                label = "Currency to use",
                choices = c("euro", "dollar", "pound", "yen"),
                selected = "euro"
              ),
              selectInput(
                inputId = "typequest",
                label = "Type of questions",
                choices = c("mcq", "open"),
                selected = "mcq"
              )
            ),
            
            fillRow(
              flex = c(1,1,1),
              textInput(
                inputId = "name",
                label = "File name",
                value = "exam"
              ),
              dateInput(
                inputId = "datexam",
                label = "Date of delivery"
              ),
              selectInput(
                inputId = "platform",
                label = "Platform of delivery",
                choices = c("Print", "Web", "Moodle", "Blackboard"),
                selected = "Print"
              )
            ),
            
            tags$hr(),
            
            fillRow(
              flex = c(1, 1, 1, 1),
              fillCol(
                flex = c(1,1,1),
                conditionalPanel(
                  'input.platform === "Print"',
                  selectInput(
                    inputId = "format",
                    label = "Format",
                    choices = c("A4", "letter"),
                    selected = "A4"
                  ),
                  checkboxInput(
                    inputId = "showquestid",
                    label = "Show question id",
                    value = FALSE
                  ),
                  checkboxInput(
                    inputId = "showquestpt",
                    label = "Show question points",
                    value = TRUE
                  )
                )
              ),
              fillCol(
                flex = c(1,1,1),
                conditionalPanel(
                  'input.platform === "Print"',
                  numericInput(
                    inputId = "versions",
                    label = "Number of versions",
                    value = 1,
                    min = 1,
                    max = 4,
                    step = 1
                  ),
                  numericInput(
                    inputId = "blocsize",
                    label = "Bloc size",
                    value = 1,
                    min = 1,
                    max = 45,
                    step = 1
                  ),
                  checkboxInput(
                    inputId = "withscan",
                    label = "With scan (limit to 45 questions)",
                    value = FALSE
                  )
                )
              ),
              fillCol(
                flex = c(1,1,1),
                conditionalPanel(
                  'input.withscan',
                  textInput(
                    inputId = "institution",
                    label = "Institution name",
                    value = ""
                  ),
                  textInput(
                    inputId = "title",
                    label = "Title of the course",
                    value = ""
                  ),
                  numericInput(
                    inputId = "reglength",
                    label = "Length student ID",
                    value = 7,
                    min = 7,
                    max = 9,
                    step = 1
                  )
                )
              ),
              conditionalPanel(
                'input.typequest === "mcq"',
                numericInput(
                  inputId = "alternatives",
                  label = "Number of alternatives",
                  value = 5,
                  min = 4,
                  max = 5,
                  step = 1
                )
              )
            )
          )
        )
      ),
      
      
      miniTabPanel("Select",
        icon = icon("filter"),
        miniContentPanel(
          fillRow(
            flex = c(1,1),
            fillCol(
              flex = c(4, 1, 2, 5),
              fillRow(
                flex = c(1, 1),
                fillCol(
                  flex = c(1, 1, 1, 1),
                  uiOutput(outputId = "filtchapter"),
                  uiOutput(outputId = "filtsection"),
                  uiOutput(outputId = "filtsubsection"),
                  uiOutput(outputId = "filtobjective")
                ),
                fillCol(
                  flex = c(1, 1, 1, 1),
                  uiOutput(outputId = "filtlevel"),
                  uiOutput(outputId = "filtbloom"),
                  uiOutput(outputId = "filtdifficulty"),
                  textInput("filtkeyword", "Keywords", value = "")
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
                    step = 1
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
      ),
      
      
      miniTabPanel("Sort",
        icon = icon("list-ol"),
        miniContentPanel(
          DT::DTOutput(outputId = "contentexam", height = "600px"),
          tags$hr(),
          uiOutput(outputId = "order"),
          fillRow(
            flex = c(4, 1, 1),
            textOutput(outputId = "totalPoints"),
            actionButton(
              inputId = "reorder",
              label = "Remove and reorder",
              width = "100%",
              icon("list-ol")
            )
          )
        )
      ),
      
      
      miniTabPanel("Check",
                   icon = icon("eye"),
                   miniContentPanel(
                     uiOutput("viewquest")
                   )
      ),
      
      
      miniTabPanel("Balance",
                   icon = icon("balance-scale"),
                   miniContentPanel(
                     fillCol(
                       flex = c(1,1,10),
                       fillRow(flex = c(1,1),
                               selectInput("tblrow", "Select rows",
                                           choices = c("L1","L2","L3"),
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
    Bloom <- NULL
    ID <- NULL
    KD <- NULL
    LS <- NULL
    PT <- NULL
    QN <- NULL
    SD <- NULL
    Topic <- NULL
    Value <- NULL
    bloom <- NULL
    chapter <- NULL
    difficulty <- NULL
    kind <- NULL
    language <- NULL
    level <- NULL
    objective <- NULL
    paths <- NULL
    questionID <- NULL
    score <- NULL
    section <- NULL
    subsection <- NULL
    bloc <- NULL
    
    
    ################
    # Filter list and selection table creation
    
    tables <- reactiveValues()

    # Visibly bind variables (avoid notes in checks)

    observe({
      if (is.null(input$selection)) {
        tables$contentexam <- data.frame(
          ID = 0,
          QN = "tmp",
          LG = "tmp",
          L1 = "tmp",
          L2 = "tmp",
          L3 = "tmp",
          LO = "tmp",
          LV = "tmp",
          BL = "tmp",
          DI = 0,
          KD = "tmp",
          PT = 0,
          SD = 0,
          stringsAsFactors = FALSE
        )
      } else {
        tables$contentexam <- as.data.frame(
          utils::read.csv(
            file = input$selection$datapath[[1]],
            stringsAsFactors = FALSE
          ),
          stringsAsFactors = FALSE
        )
      }
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
      tables$order <- c()
    })
    
    
    questionlist <- reactive({
      questions <- questions
      if (!is.null(input$language) & !is.null(input$typequest)){
        questions <- subset(questions, questions$language == input$language)
        questions <- subset(questions, questions$kind %in% c(input$typequest,"both"))
        questions <- subset(questions, !(questions$questionID %in% tables$exclusion$QN))
      }
      questions
    })

    
    
    ####################
    # Prepare filters
    output$filtchapter <- renderUI({
      choices <- sort(c(setdiff(unique(questionlist()$chapter), ""), ""), decreasing = FALSE)
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
        questionlist()
      } else if (filter == "") {
        questionlist()
      } else {
        subset(questionlist(), stringr::str_detect(questionlist()$chapter, filter))
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
    
    
    
    output$filtobjective <- renderUI({
      choices <- sort(c(setdiff(unique(afterfiltsubsection()$objective), ""), ""), decreasing = FALSE)
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
        afterfiltsubsection()
      } else if (filter == "") {
        afterfiltsubsection()
      } else {
        subset(afterfiltsubsection(), stringr::str_detect(afterfiltsubsection()$objective, filter))
      }
    })
    
    
    
    output$filtlevel <- renderUI({
      choices <- sort(c(setdiff(unique(afterfiltobjective()$level), ""), ""), decreasing = FALSE)
      selectInput("slctlevel",
                  "Level:",
                  choices = choices,
                  selected = "",
                  multiple = FALSE,
                  width = '80%')
    })
    
    afterfiltlevel <- reactive({
      filter <- input$slctlevel
      if (is.null(filter)){
        afterfiltobjective()
      } else if (filter == "") {
        afterfiltobjective()
      } else {
        subset(afterfiltobjective(), stringr::str_detect(afterfiltobjective()$level, filter))
      }
    })
    
    
    
    output$filtbloom <- renderUI({
      choices <- sort(c(setdiff(unique(afterfiltlevel()$bloom), ""), ""), decreasing = FALSE)
      selectInput("slctbloom",
                  "Bloom:",
                  choices = choices,
                  selected = "",
                  multiple = FALSE,
                  width = '80%')
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
                  multiple = FALSE,
                  width = '80%')
    })
    
    afterfiltdificulty <- reactive({
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
        afterfiltdificulty()
      } else if (filter[[1]] == "") {
        afterfiltdificulty()
      } else {
        keywords <- stringr::str_to_lower(unlist(str_split(filter, " ")))
        keywords <- stringr::str_replace_all(keywords, "_", " ")
        base <- afterfiltdificulty()
        for (i in 1:length(keywords)) base <- subset(afterfiltdificulty(), stringr::str_detect(stringr::str_to_lower(afterfiltdificulty()$description), keywords[i]))
        base
      }
    })
    
    
    
    preselection <- reactive({
      reset <- input$addSample

      if (input$allowduplicates == FALSE) {
        available <- setdiff(afterfiltkeyword()$questionID, tables$contentexam$QN)
      } else {
        available <- afterfiltkeyword()$questionID
      }

      preselection <- subset(afterfiltkeyword(), afterfiltkeyword()$questionID %in% available)

      preselection
    })
    

    ##########################################
    # Display tables

    output$manslct <- renderUI({
      if (!is.null(preselection())){
        preselection <- preselection()$questionID
        selectInput(
          inputId = "manslct",
          label = "Sample",
          choices = preselection,
          selected = preselection,
          multiple = TRUE,
          selectize = TRUE,
          width = "80%"
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
        address <- system.file("doc", paste0(input$slctexample, ".html"), package=input$package_name)
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
          address <- system.file("doc", paste0(question, ".html"), package=input$package_name)
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
          dplyr::filter(questionID == question) %>%
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
            L1 = chapter,
            L2 = section,
            L3 = subsection,
            LO = objective,
            LV = level,
            BL = bloom,
            DI = difficulty,
            KD = kind,
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
          L1 = "tmp",
          L2 = "tmp",
          L3 = "tmp",
          LO = "tmp",
          LV = "tmp",
          BL = "tmp",
          DI = "tmp",
          KD = "tmp",
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

        # Save parameters accessibles for question generation
        if (!is.null(input$typequest)) type_quest <- input$typequest else type_quest <- "mcq"
        if (type_quest == "mcq") {
          choices <- dplyr::filter(choices, KD == "mcq" | KD == "both")
        } else if (type_quest == "open") {
          choices <- dplyr::filter(choices, KD == "open" | KD == "both")
        }

        if (!is.null(input$showquestid)) show_question_id <- input$showquestid else show_question_id <- FALSE
        if (!is.null(input$showquestpt)) show_question_pt <- input$showquestpt else show_question_pt <- FALSE

        # Remove question unfit for lms if lms
        if (input$platform %in% c("Blackboard", "Moodle")) choices <- dplyr::filter(choices, LS == "yes") else choices <- choices

        # Define the kind of table
        if (input$platform %in% c("Web", "Blackboard", "Moodle")) type_table <- "html" else type_table <- "latex"
        wd <- getwd()

        currency <- input$currency
        alternatives <- input$alternatives

        save(type_quest, show_question_id, show_question_pt, choices, type_table, currency, alternatives, file = paste0(wd, "/parameters/exam_parameters.RData"))

        # Set paths to exercises and templates
        exercises <- paste0(find.package(input$package_name), "/rmd")
        templates <- paste0(find.package(input$package_name), "/tex")

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
        
        examid <- paste0(
          substring(gsub("-", "", input$datexam), 3)
        )


        # Generate the exam
        if (input$platform == "Web") {
          incProgress(1 / 2, detail = "Generating...")
          web <- exams::exams2html(myexam,
            n = 1,
            dir = "web",
            edir = exercises,
            tdir = tmpdir,
            name = examid,
            mathjax = TRUE
          )
          
        } else if (input$platform == "Blackboard") {
          incProgress(1 / 2, detail = "Generating...")
          moodle <- exams::exams2blackboard(myexam,
            n = 1,
            dir = "blackboard",
            edir = exercises,
            tdir = tmpdir,
            zip = ifelse(Sys.info()[[1]] == "Windows", FALSE, TRUE),
            name = examid
          )
          
        } else if (input$platform == "Moodle") {
          incProgress(1 / 2, detail = "Generating...")
          moodle <- exams2moodle(myexam,
            n = 1,
            dir = "moodle",
            edir = exercises,
            tdir = tmpdir,
            name = examid
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
            
            # Set the ewam id
            versionid <- paste0(
              examid,
              paste0(rep(0, (3 - nchar(as.character(i)))), collapse = ""),
              i
            )
            
            header <- list(Date = input$datexam, ID = versionid)
            
            
            
            set.seed(seed)
            exam <- exams::exams2pdf(
              mutate(myexams[[i]], ES = "exam"),
              n = 1,
              dir = "questions",
              edir = exercises,
              tdir = tmpdir,
              texdir = tmpdir,
              name = paste0("questions_", versionid),
              header = header,
              points = choices$PT,
              template = paste0(templates, "/exam_", input$language, "_", input$typequest, "_", input$format, ".tex")
            )
            
            unlink(paste0(tmpdir, "/*"))
            
            set.seed(seed)
            solu <- exams::exams2pdf(
              mutate(myexams[[i]], ES = "solution"),
              n = 1,
              dir = "questions",
              edir = exercises,
              tdir = tmpdir,
              texdir = tmpdir,
              name = paste0("solutions_", versionid),
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
        
        utils::write.csv(exportexam, paste0("parameters/questions_", examid, ".csv"), row.names = FALSE)
        
        if (input$versions > 1){
          exportblocs <- prepversions %>%
            dplyr::bind_rows() %>%
            dplyr::select(bloc, question = QN) %>%
            unique()
          
          utils::write.csv(exportblocs, paste0("parameters/blocs_", examid,".csv"), row.names = FALSE)
          utils::write.csv(orderversions, paste0("parameters/versions_", examid,".csv"), row.names = FALSE)
        }
        
      })

      stopApp()
    })
  }
  
  runGadget(ui, server, viewer = dialogViewer(dialogName = "genexam", width = 1800, height = 1200))
}
