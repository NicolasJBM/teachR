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
#' @importFrom shiny dataTableOutput
#' @importFrom shiny tableOutput
#' @importFrom shiny uiOutput
#' @importFrom shiny plotOutput
#' @importFrom shiny actionButton
#' @importFrom shiny renderDataTable
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
#' @importFrom tibble column_to_rownames
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize_all
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
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
#' @importFrom tth tth
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom webshot webshot
#' @importFrom stats na.omit
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom utils data
#' @importFrom utils read.csv
#' @importFrom writR stat_totals
#' @export


genexam <- function() {
  ui <- miniPage(
    gadgetTitleBar("Generator of Exams and Solutions"),
    miniTabstripPanel(
      miniTabPanel("Define",
        icon = icon("sliders"),
        miniContentPanel(
          fillCol(
            flex = c(1, 1, 1, 1, 1, 1, 5, 1),
            fillRow(
              flex = c(1, 1, 1),
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
            tags$hr(),
            fillRow(
              flex = c(1, 1),
              selectInput(
                inputId = "language",
                label = "Language",
                choices = c("en", "fr", "de", "es", "it", "nl"),
                selected = "en"
              ),
              selectInput(
                inputId = "currency",
                label = "Currency to use",
                choices = c("euro", "dollar", "pound", "yen"),
                selected = "euro"
              )
            ),
            fillRow(
              flex = c(1, 1),
              textInput(
                inputId = "name",
                label = "File name (no space or special characters)",
                value = "exam"
              ),
              numericInput(
                inputId = "version",
                label = "Version id",
                value = 1,
                min = 1,
                max = 100,
                step = 1
              )
            ),
            fillRow(
              flex = c(1, 1),
              selectInput(
                inputId = "format",
                label = "Format",
                choices = c("A4", "letter"),
                selected = "A4"
              ),
              dateInput(
                inputId = "datexam",
                label = "Date of delivery"
              )
            ),
            tags$hr(),
            fillRow(
              flex = c(1, 1),
              fillCol(
                flex = c(1, 4),
                selectInput(
                  inputId = "platform",
                  label = "Platform of delivery",
                  choices = c("Print", "Web", "Moodle", "Blackboard"),
                  selected = "Print"
                ),
                conditionalPanel(
                  'input.platform === "Print"',
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
                  checkboxInput(
                    inputId = "withscan",
                    label = "With scan (limit to 45 questions)",
                    value = TRUE
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
                'input.platform === "Print"',
                selectInput(
                  inputId = "typequest",
                  label = "Type of questions",
                  choices = c("Multiple choice", "Open"),
                  selected = "Multiple choice"
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
                ),
                conditionalPanel(
                  'input.typequest === "Multiple choice"',
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
        )
      ),
      miniTabPanel("Select",
        icon = icon("filter"),
        miniContentPanel(
          fillCol(
            flex = c(7, 1, 6),
            fillRow(
              flex = c(1, 1),
              fillCol(
                flex = c(1, 1, 1, 1),
                uiOutput(outputId = "chapter"),
                conditionalPanel(
                  'input.chapter != "0 Any"',
                  uiOutput(outputId = "section")
                ),
                conditionalPanel(
                  'input.chapter != "0 Any" & input.section != "0 Any"',
                  uiOutput(outputId = "subsection")
                ),
                conditionalPanel(
                  'input.chapter != "0 Any" & input.section != "0 Any" & input.subsection != "0 Any"',
                  uiOutput(outputId = "objective")
                )
              ),
              fillCol(
                flex = c(1, 1, 1, 1),
                uiOutput(outputId = "level"),
                uiOutput(outputId = "bloom"),
                uiOutput(outputId = "difficulty"),
                textInput("keywords", "Keywords", value = "")
              )
            ),
            tags$hr(),
            fillRow(
              flex = c(3, 1),
              uiOutput("sampled"),
              fillCol(
                flex = c(1, 1, 1),
                checkboxInput(
                  inputId = "allowduplicates",
                  label = "Allow duplicates",
                  value = FALSE
                ),
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
                  width = "100%",
                  icon("filter"),
                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                )
              )
            )
          )
        )
      ),
      miniTabPanel("Look",
                   icon = icon("eye"),
                   miniContentPanel(
                     uiOutput("slctquest"),
                     uiOutput("lookquest")
                   )
      ),
      miniTabPanel("Sort",
        icon = icon("list-ol"),
        miniContentPanel(
          dataTableOutput(outputId = "contentexam"),
          tags$hr(),
          uiOutput(outputId = "order"),
          fillRow(
            flex = c(1, 3),
            textOutput(outputId = "totalPoints"),
            actionButton(
              inputId = "reorder",
              label = "Remove and reorder",
              width = "100%",
              icon("list-ol"),
              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
          )
        )
      ),
      miniTabPanel("Check",
                   icon = icon("check"),
                   miniContentPanel(
                     uiOutput("takequest"),
                     uiOutput("viewquest")
                   )
      ),
      miniTabPanel("Balance",
                   icon = icon("balance-scale"),
                   miniContentPanel(
                     selectInput("tblrow", "Select rows", choices = c("L1","L2","L3"), selected = "L1"),
                     selectInput("tblval", "Select values", choices = c("points","questions","difficulty"), selected = "points"),
                     tags$hr(),
                     tableOutput("balance")
                   )
      )
    )
  )
  

  server <- function(input, output, session) {

    # Bind variables
    Topic <- NULL
    Bloom <- NULL
    Value <- NULL
    
    ################
    # Filter list and selection table creation

    tables <- reactiveValues()

    # Visibly bind variables (avoid notes in checks)
    ID <- NULL
    PT <- NULL
    QN <- NULL
    SD <- NULL
    LG <- NULL
    L1 <- NULL
    L2 <- NULL
    L3 <- NULL
    LO <- NULL
    LV <- NULL
    BL <- NULL
    DI <- NULL
    LS <- NULL
    KD <- NULL
    bloom <- NULL
    chapter <- NULL
    data <- NULL
    description <- NULL
    difficulty <- NULL
    language <- NULL
    level <- NULL
    paths <- NULL
    questionID <- NULL
    section <- NULL
    subsection <- NULL
    objective <- NULL
    lms <- NULL
    kind <- NULL
    score <- NULL

    observe({
      if (is.null(input$selection)) {
        tables$contentexam <- tibble(
          ID = 0,
          LG = "tmp",
          L1 = "tmp",
          L2 = "tmp",
          L3 = "tmp",
          LO = "tmp",
          LV = "tmp",
          BL = "tmp",
          DI = 0,
          PT = 0,
          SD = 0,
          LS = "tmp",
          KD = "tmp",
          QN = "tmp"
        )
      } else {
        tables$contentexam <- as_tibble(
          read.csv(file = input$selection$datapath[[1]], stringsAsFactors = FALSE)
        )
      }
      
      if (is.null(input$exclusion)) {
        tables$exclusion <- tibble(
          QN = ""
        )
      } else {
        tables$exclusion <- as_tibble(
          read.csv(file = input$exclusion$datapath[[1]], stringsAsFactors = FALSE)
        )
      }
      
      tables$order <- c()
    })


    ####################
    # Selection of questions and update of subfilters
    questions <- reactive({
      questions <- get(paste0(input$package_name, "_questions"))
      questions <- dplyr::filter(questions, !(questionID %in% tables$exclusion$QN))
      questions
    })

    preselection <- reactive({
      reset <- input$addSample

      if (input$allowduplicates == FALSE) {
        available <- setdiff(questions()$questionID, tables$contentexam$QN)
      } else {
        available <- questions()$questionID
      }

      preselection <- dplyr::filter(questions(), questionID %in% available)
      if (input$typequest == "Multiple choice") {
        preselection <- dplyr::filter(preselection, kind == "mcq" | kind == "both")
      } else {
        preselection <- dplyr::filter(preselection, kind == "open" | kind == "both")
      }

      if (!is.null(input$language)) sellanguage <- input$language else sellanguage <- "Any"
      if (!is.null(input$chapter)) selchapter <- input$chapter else selchapter <- "0 Any"
      if (!is.null(input$section)) selsection <- input$section else selsection <- "0 Any"
      if (!is.null(input$subsection)) selsubsection <- input$subsection else selsubsection <- "0 Any"
      if (!is.null(input$objective)) selobjective <- input$objective else selobjective <- "0 Any"
      if (!is.null(input$level)) sellevel <- input$level else sellevel <- "0 Any"
      if (!is.null(input$bloom)) selbloom <- input$bloom else selbloom <- "0 Any"
      if (!is.null(input$difficulty)) seldifficulty <- input$difficulty else seldifficulty <- "Any"
      if (input$keywords != "") selkeywords <- unlist(stringr::str_split(input$keywords, " ")) else selkeywords <- c(NA)

      if (sellanguage != "Any") preselection <- dplyr::filter(preselection, language == sellanguage)
      if (selchapter != "0 Any") {
        preselection <- dplyr::filter(preselection, chapter == selchapter)
        if (selsection != "0 Any") {
          preselection <- dplyr::filter(preselection, section == selsection)
          if (selsubsection != "0 Any") {
            preselection <- dplyr::filter(preselection, subsection == selsubsection)
            if (selobjective != "0 Any") {
              preselection <- dplyr::filter(preselection, objective == selobjective)
            }
          }
        }
      }
      if (sellevel != "0 Any") preselection <- dplyr::filter(preselection, level == sellevel)
      if (selbloom != "0 Any") preselection <- dplyr::filter(preselection, bloom == selbloom)
      if (seldifficulty != "Any") preselection <- dplyr::filter(preselection, difficulty == seldifficulty)
      
      if (!is.na(selkeywords[[1]])){
        for (i in 1:length(selkeywords)){
          preselection <- preselection %>%
            filter(stringr::str_detect(description, pattern = selkeywords[[i]]))
        } 
      }

      preselection
    })

    sampled <- reactive({
      preselection()$questionID
    })

    ##########################
    # Menus' dynamic generation after filters' update
    output$chapter <- renderUI({
      selection <- c("0 Any", sort(unique(preselection()$chapter)))
      if (!is.null(input$chapter)) selected <- input$chapter else selected <- "0 Any"
      selectInput(
        inputId = "chapter",
        label = "Chapter",
        choices = selection,
        selected = selected
      )
    })

    output$section <- renderUI({
      selection <- c("0 Any", sort(unique(preselection()$section)))
      if (!is.null(input$section)) selected <- input$section else selected <- "0 Any"
      selectInput(
        inputId = "section",
        label = "Section",
        choices = selection,
        selected = selected
      )
    })

    output$subsection <- renderUI({
      selection <- c("0 Any", sort(unique(preselection()$subsection)))
      if (!is.null(input$subsection)) selected <- input$subsection else selected <- "0 Any"
      selectInput(
        inputId = "subsection",
        label = "Subsection",
        choices = selection,
        selected = selected
      )
    })

    output$objective <- renderUI({
      selection <- c("0 Any", sort(unique(preselection()$objective)))
      if (!is.null(input$objective)) selected <- input$objective else selected <- "0 Any"
      selectInput(
        inputId = "objective",
        label = "Objective",
        choices = selection,
        selected = selected
      )
    })

    output$level <- renderUI({
      selection <- c("0 Any", sort(unique(preselection()$level)))
      if (!is.null(input$level)) selected <- input$level else selected <- "0 Any"
      selectInput(
        inputId = "level",
        label = "Level",
        choices = selection,
        selected = selected
      )
    })

    output$bloom <- renderUI({
      selection <- c("0 Any", sort(unique(preselection()$bloom)))
      if (!is.null(input$bloom)) selected <- input$bloom else selected <- "0 Any"
      selectInput(
        inputId = "bloom",
        label = "Bloom",
        choices = selection,
        selected = selected
      )
    })

    output$difficulty <- renderUI({
      selection <- c("Any", sort(unique(preselection()$difficulty)))
      if (!is.null(input$difficulty)) selected <- input$difficulty else selected <- "Any"
      selectInput(
        inputId = "difficulty",
        label = "Difficulty",
        choices = selection,
        selected = selected
      )
    })

    ##########################################
    # Display tables

    output$sampled <- renderUI({
      sampled <- sampled()
      selectInput(
        inputId = "sampled",
        label = "Sample",
        choices = sampled,
        selected = sampled,
        multiple = TRUE,
        selectize = TRUE,
        width = "80%"
      )
    })

    output$contentexam <- renderDataTable(
      tables$contentexam,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 15), searching = FALSE, xtable.include.rownames = TRUE)
    )

    output$totalPoints <- renderText({
      paste0("Total points: ", sum(tables$contentexam$PT))
    })

    output$order <- renderUI({
      order <- tables$contentexam$ID
      selectInput("order", "Order", choices = order, selected = order, multiple = TRUE, width = "100%")
    })
    
    output$slctquest <- renderUI({
      ids <- input$sampled
      selectInput("slctquest",
                  "Select a question",
                  choices = ids,
                  selected = ids[1])
    })
    
    output$lookquest <- renderUI({
      if (!is.null(input$slctquest)){
        file <- paste0(input$slctquest, ".html")
        address <- system.file("doc", file, package=input$package_name)
        withMathJax(includeHTML(address))
      }
    })
    
    output$takequest <- renderUI({
      if (length(tables$contentexam$QN) > 0) {
        ids <- tables$contentexam$QN
      } else questions$questionID
      selectInput("selquest",
                  "Select a question",
                  choices = ids,
                  selected = ids[1])
    })
    
    output$viewquest <- renderUI({
      if (!is.null(input$selquest)){
        file <- paste0(input$selquest, ".html")
        address <- system.file("doc", file, package=input$package_name)
        withMathJax(includeHTML(address))
      }
    })
    
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

      if (!is.null(input$sampled)) {
        question <- base::sample(input$sampled, 1)
        questionNbr <- nrow(table) + 1

        add <- preselection() %>%
          dplyr::filter(questionID == question) %>%
          dplyr::mutate(
            ID = questionNbr,
            PT = input$points,
            SD = sample(c(1:9999), 1),
            QN = question
          ) %>%
          dplyr::select(
            ID,
            LG = language,
            L1 = chapter,
            L2 = section,
            L3 = subsection,
            LO = objective,
            LV = level,
            BL = bloom,
            DI = difficulty,
            PT,
            SD,
            LS = lms,
            KD = kind,
            QN
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
        tables$contentexam <- tibble(
          ID = 0, LG = "tmp",
          L1 = "tmp", L2 = "tmp", L3 = "tmp", LO = "tmp",
          LV = "tmp", BL = "tmp", DI = "tmp",
          PT = 0, SD = 0, LS = "tmp", KD = "tmp", QN = "tmp"
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
        if (!is.null(input$typequest)) type_quest <- input$typequest else type_quest <- "Multiple choice"
        if (type_quest == "Multiple choice") {
          choices <- dplyr::filter(choices, KD == "mcq" | KD == "both")
        } else {
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
          dplyr::mutate(paths = map(QN, function(x, suffix) paste0(x, suffix), suffix = suffix))
        myexam <- choices$paths
        
        # Create the directory in which all files are generated
        ifelse(!dir.exists(file.path(wd, "tmp")), dir.create(file.path(wd, "tmp")), FALSE)
        tmpdir <- paste0(wd, "/tmp")
        unlink(paste0(tmpdir, "/*"))

        # Set the ewam id
        examid <- paste0(
          substring(gsub("-", "", input$datexam), 3),
          paste0(rep(0, 5 - nchar(as.character(input$version))), collapse = ""),
          input$version
        )
        
        if (input$typequest == "Multiple choice") stypequest <- "mcq" else stypequest <- "open"


        # Generate the exam
        if (input$platform == "Web") {
          incProgress(1 / 2, detail = "Generating...")

          web <- exams2html(myexam,
            n = 1,
            dir = "web",
            edir = exercises,
            tdir = tmpdir,
            name = examid,
            mathjax = TRUE
          )
        } else if (input$platform == "Blackboard") {
          incProgress(1 / 2, detail = "Generating...")

          moodle <- exams2blackboard(myexam,
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
          incProgress(1 / 4, detail = "Generating questions...")

          header <- list(Date = input$datexam, ID = examid)
          seed <- sample(c(1:9999), 1)

          set.seed(seed)
          exam <- exams2pdf(myexam,
            n = 1,
            dir = "questions",
            edir = exercises,
            tdir = tmpdir,
            texdir = tmpdir,
            name = paste0(examid, "_questions"),
            header = header,
            points = choices$PT,
            template = paste0(templates, "/exam_", input$language, "_", stypequest, "_", input$format, ".tex")
          )

          incProgress(2 / 4, detail = "Generating solutions...")

          unlink(paste0(tmpdir, "/*"))
          set.seed(seed)
          solu <- exams2pdf(myexam,
            n = 1,
            dir = "questions",
            edir = exercises,
            tdir = tmpdir,
            texdir = tmpdir,
            name = paste0(examid, "_solutions"),
            header = header,
            template = paste0(templates, "/solution_", input$language, "_", stypequest, "_", input$format, ".tex")
          )


          if (stypequest == "mcq" & input$withscan) {
            incProgress(3 / 4, detail = "Generating scan...")

            if (input$format == "A4") {
              teachR::make_scan_A4(
                exam = exam,
                name = examid,
                language = input$language,
                title = input$title,
                institution = input$institution,
                date = input$datexam,
                startid = input$version,
                alternatives = input$alternatives,
                reglength = input$reglength,
                encoding = ""
              )
            } else {

            }
          }
        }

        choices <- choices %>%
          dplyr::select(-paths) %>%
          as.data.frame()
        
        if (stypequest == "open") {
          grading_ <- tibble(
            student_ID = NA,
            question = choices$ID,
            score = NA
          ) %>%
            tidyr::spread(question, score, fill = NA)
          write.csv(grading_, paste0("answers/grading_", examid, ".csv"), row.names = FALSE)
        }

        write.csv(choices, paste0("parameters/", examid, ".csv"), row.names = FALSE)
      })

      stopApp()
    })
  }
  runGadget(ui, server)
}
