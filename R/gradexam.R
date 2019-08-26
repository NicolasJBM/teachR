#' Shiny gadget to gather informations about questions, participants, and answers.
#' Then you can quickly analyze results across versions and questions.
#' @seealso genexam()
#' @seealso checkexam()
#' @seealso sendexam()
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
#' @importFrom shiny tags
#' @importFrom shiny dataTableOutput
#' @importFrom shiny uiOutput
#' @importFrom shiny plotOutput
#' @importFrom shiny actionButton
#' @importFrom shiny renderDataTable
#' @importFrom shiny renderUI
#' @importFrom shiny renderPlot
#' @importFrom shiny renderText
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny withProgress
#' @importFrom shiny incProgress
#' @importFrom shiny fluidPage
#' @importFrom shiny h3
#' @importFrom shiny h5
#' @importFrom shiny tabsetPanel
#' @importFrom shiny tabPanel
#' @importFrom shiny sidebarLayout
#' @importFrom shiny sidebarPanel
#' @importFrom shiny mainPanel
#' @importFrom shiny sliderInput
#' @importFrom shiny eventReactive
#' @importFrom tibble column_to_rownames
#' @importFrom tibble tibble
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
#' @importFrom tidyr separate
#' @importFrom tidyr unite
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#' @importFrom exams nops_scan
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom WriteXLS WriteXLS
#' @importFrom utils write.csv
#' @importFrom stats na.omit
#' @importFrom utils data
#' @importFrom png readPNG
#' @importFrom png writePNG
#' @export


gradexam <- function() {
  ui <- fluidPage(
    h3("Gather and Compile Exam Results"),
    h5("Please open in your html browser and follow the steps."),
    tabsetPanel(
      tabPanel(
        "1. Gather test information",
        sidebarLayout(
          sidebarPanel(
            fileInput(
              inputId = "slct_files",
              label = "Selected questions (.xlsx)",
              accept = c(".xlsx"),
              multiple = TRUE
            ),
            fileInput(
              inputId = "exam_files",
              label = "Exam files (.rds)",
              accept = c(".rds"),
              multiple = TRUE
            ),
            actionButton(
              inputId = "gather",
              label = "Gather information"
            )
          ),
          mainPanel(
            dataTableOutput(outputId = "questions")
          )
        )
      ),
      tabPanel(
        "2. Gather exam results",
        sidebarLayout(
          sidebarPanel(
            fileInput(
              inputId = "part_file",
              label = "Enrolled participants (.xlsx)",
              accept = c(".xlsx"),
              multiple = FALSE
            ),
            fileInput(
              inputId = "scan_files",
              label = "Answer sheets (.png)",
              accept = c(".png"),
              multiple = TRUE
            ),
            sliderInput(
              inputId = "threshold",
              label = "Scan threshold",
              min = 0.05,
              max = 0.65,
              value = c(0.1, 0.55)
            ),
            numericInput(
              inputId = "minrot",
              label = "Scan rotation",
              value = 0.005,
              min = 0.001,
              max = 0.009,
              step = 0.001
            ),
            numericInput(
              inputId = "correction",
              label = "Guess correction",
              value = 0,
              min = 0,
              max = 1,
              step = 0.01
            ),
            actionButton(
              inputId = "scan",
              label = "Scan answer sheets"
            )
          ),
          mainPanel(
            dataTableOutput(outputId = "answers")
          )
        )
      ),
      tabPanel(
        "3. Compile and save",
        downloadButton(
          outputId = "dwldresults",
          label = "Save results"
        ),
        dataTableOutput(outputId = "results")
      ),
      tabPanel(
        "4. Save non-identified participants",
        downloadButton(
          outputId = "dwldunknown",
          label = "Save unknown"
        ),
        dataTableOutput(outputId = "unkown")
      ),
      tabPanel(
        "3. Save missing participants",
        downloadButton(
          outputId = "dwldmissing",
          label = "Save missing"
        ),
        dataTableOutput(outputId = "missing")
      )
    )
  )


  server <- function(input, output, session) {

    #################
    # Visibly bind variables
    QN <- NULL
    ID <- NULL
    LG <- NULL
    LS <- NULL
    KD <- NULL
    PT <- NULL
    L1 <- NULL
    L2 <- NULL
    L3 <- NULL
    LO <- NULL
    LV <- NULL
    BL <- NULL
    SD <- NULL
    DI <- NULL
    solution <- NULL
    answer <- NULL
    file <- NULL
    scrambling <- NULL
    type <- NULL
    check <- NULL
    solution <- NULL
    student_id <- NULL
    question <- NULL
    scrambling <- NULL
    type <- NULL
    check <- NULL
    answers <- NULL
    correct <- NULL
    factor <- NULL

    #################

    questions <- eventReactive(input$gather, {
      if (!is.null(input$slct_files) &
        !is.null(input$exam_files)) {

        # Questions
        selected_questions <- list()
        for (i in 1:nrow(input$slct_files)) {
          tmp <- read_excel(input$slct_files$datapath[[i]])
          exam_id <- gsub(".xlsx", "", input$slct_files$name[[i]])
          selected_questions[[i]] <- tmp %>%
            dplyr::mutate(exam_id = rep(exam_id, nrow(tmp)))
        }
        selected_questions <- dplyr::bind_rows(selected_questions) %>%
          dplyr::select(exam_id, question_id = QN, everything())


        # Solutions
        exams_solutions <- list()
        for (i in 1:nrow(input$exam_files)) {
          exam <- readRDS(input$exam_files$datapath[[i]])
          exam_id <- gsub(".rds", "", input$exam_files$name[[i]])
          questnbr <- length(exam[[1]])
          questions <- c()
          points <- c()
          solution <- c()
          for (j in 1:questnbr) questions[j] <- exam$exam1[[j]]$metainfo$name
          for (j in 1:questnbr) points[j] <- exam$exam1[[j]]$metainfo$points
          for (j in 1:questnbr) solution[j] <- paste0(as.numeric(exam$exam1[[j]]$metainfo$solution), collapse = "")
          solutions <- tibble(
            exam_id = rep(exam_id, length(questions)),
            question = paste0("Q", c(1:questnbr)),
            question_id = questions,
            points = points,
            solution = solution
          )
          exams_solutions[[i]] <- solutions
        }
        exams_solutions <- dplyr::bind_rows(exams_solutions) %>%
          dplyr::mutate(solution = map(solution, paste0, "0")) %>%
          dplyr::mutate(solution = map(solution, binary_to_letter)) %>%
          dplyr::mutate(solution = unlist(solution))

        questions <- selected_questions %>%
          dplyr::left_join(exams_solutions, by = c("exam_id", "question_id")) %>%
          dplyr::mutate(exam_id = as.character(exam_id))
      } else {
        questions <- NULL
      }

      questions
    })

    output$questions <- renderDataTable({
      questions()
    })
    output$participants <- renderDataTable({
      participants()
    })


    answers <- eventReactive(input$scan, {
      if (!is.null(input$part_file) & !is.null(input$scan_files) & !is.null(questions())) {
        withProgress(message = "Retrieve information", value = 0, {
          incProgress(1 / 5, detail = "get paricipants")

          participants <- read_excel(input$part_file$datapath[[1]]) %>%
            dplyr::mutate(student_id = as.character(student_id))

          incProgress(2 / 5, detail = "get answers")

          scan <- nops_scan(
            images = input$scan_files$datapath,
            file = FALSE,
            verbose = FALSE,
            density = 600,
            minrot = input$minrot,
            threshold = c(input$threshold[1], input$threshold[2])
          )

          incProgress(3 / 5, detail = "format scans")

          present <- tibble(answers = scan) %>%
            tidyr::separate(answers, into = c("file", "exam_id", "scrambling", "type", "check", "student_id", paste0("Q", c(1:length(unique(questions()$question))))), sep = " ") %>%
            tidyr::gather(question, answer, -file, -exam_id, -scrambling, -type, -check, -student_id) %>%
            dplyr::mutate(answer = map(answer, paste0, "0")) %>%
            dplyr::mutate(answer = map(answer, binary_to_letter)) %>%
            dplyr::mutate(answer = unlist(answer)) %>%
            dplyr::select(exam_id, student_id, question, everything()) %>%
            dplyr::mutate(exam_id = as.character(exam_id), student_id = as.character(student_id))


          incProgress(4 / 5, detail = "rename files")

          ifelse(!dir.exists(file.path(getwd(), "reports")), dir.create(file.path(getwd(), "reports")), FALSE)

          match_names <- present %>%
            dplyr::select(file, student_id) %>%
            unique() %>%
            dplyr::mutate(path = input$scan_files$datapath) %>%
            dplyr::mutate(newpath = paste0("reports/", student_id, ".png"))

          for (fil in 1:nrow(match_names)) {
            img <- readPNG(source = match_names$path[[fil]])
            writePNG(img, target = match_names$newpath[[fil]])
          }

          incProgress(5 / 5, detail = "append questions")

          present <- present %>%
            dplyr::left_join(participants, by = "student_id")

          unknown <- present %>%
            dplyr::filter(!(student_id %in% unique(participants$student_id)))

          missing <- participants %>%
            dplyr::filter(!(student_id %in% unique(present$student_id)))
        })
      } else {
        present <- NULL
        unknown <- NULL
        missing <- NULL
      }

      answers <- list(
        present = present,
        unknown = unknown,
        missing = missing
      )

      answers
    })

    output$answers <- renderDataTable({
      answers()$present
    })

    results <- reactive({
      if (!is.null(questions()) & !is.null(answers()$present)) {
        results <- questions() %>%
          dplyr::left_join(answers()$present, by = c("exam_id", "question")) %>%
          dplyr::mutate(
            factor = case_when(
              answer == "" ~ 0,
              solution == answer ~ 1,
              TRUE ~ 0 - input$correction
            )
          ) %>%
          dplyr::mutate(correct = as.numeric(factor > 0)) %>%
          dplyr::mutate(score = factor * PT)
        rmvar <- c(
          "ID", "LG", "LS", "KD", "PT",
          "file", "scrambling", "type", "check"
        )
        results <- dplyr::select(results, setdiff(names(results), rmvar)) %>%
          dplyr::rename(
            chapter = L1, section = L2, subsection = L3,
            objective = LO, level = LV, bloom = BL,
            difficulty = DI, seed = SD
          )
      } else {
        results <- NULL
      }
      results
    })

    output$results <- renderDataTable({
      results()
    })
    output$unknown <- renderDataTable({
      answers()$unknown
    })
    output$missing <- renderDataTable({
      answers()$missing
    })

    output$dwldresults <- downloadHandler(
      filename = function() {
        paste("results.xlsx")
      },
      content = function(file) {
        WriteXLS(results(), file)
      }
    )

    output$dwldunknown <- downloadHandler(
      filename = function() {
        paste("unknown.xlsx")
      },
      content = function(file) {
        WriteXLS(answers()$unknown, file)
      }
    )

    output$dwldmissing <- downloadHandler(
      filename = function() {
        paste("missing.xlsx")
      },
      content = function(file) {
        WriteXLS(answers()$missing, file)
      }
    )
  }
  runGadget(ui, server)
}


binary_to_letter <- function(x) {
  paste(c("a", "b", "c", "d", "e")[as.logical(as.numeric(unlist(strsplit(x, split = ""))))], collapse = "")
}
