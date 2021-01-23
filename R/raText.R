#' @name raText
#' @title Grade open-ended answers
#' @author Nicolas Mangin
#' @description Shiny gadget to facilitate grading open-ended questions and essays.
#' @return Three .csv files: one with students' points, one with updated solutions, one with updated criteria.
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
#' @importFrom shiny sliderInput
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
#' @importFrom shiny radioButtons
#' @importFrom shiny textAreaInput
#' @importFrom shinythemes shinytheme
#' @importFrom readODS read_ods
#' @importFrom dplyr count
#' @importFrom lexR count_words
#' @importFrom psych pca
#' @importFrom psych fa
#' @export



raText <- function() {
  ui <- miniPage(
    theme = shinythemes::shinytheme("flatly"),

    tags$head(tags$style(
      HTML(".shiny-notification {
              position:fixed;top: 30%;left: 0%;right: 0%;
           }")
    )),

    gadgetTitleBar("Grade open-ended questions"),
    miniTabstripPanel(
      miniTabPanel(
        "Upload",
        icon = icon("upload"),
        miniContentPanel(
          fillCol(
            flex = c(1, 1, 1, 1, 7),
            fillRow(
              flex = c(1, 1, 1),
              radioButtons(
                "creres",
                "Create or resume a project",
                choices = c("Create", "Resume"),
                selected = " Create",
                inline = TRUE
              ),
              tags$br(),
              actionButton(
                "import",
                "Import",
                style = "width:150px;"
              )
            ),
            tags$hr(),
            conditionalPanel(
              condition = "input.creres === 'Create'",
              fillRow(
                flex = c(1, 1, 1),
                fileInput(
                  "answers",
                  "Import answers",
                  multiple = FALSE,
                  accept = c("ods")
                ),
                fileInput(
                  "criteria",
                  "Import criteria",
                  multiple = FALSE,
                  accept = c(".ods")
                ),
                fileInput(
                  "solutions",
                  "Import solutions",
                  multiple = FALSE,
                  accept = c(".ods")
                )
              )
            ),
            conditionalPanel(
              condition = "input.creres === 'Resume'",
              fileInput(
                "backup",
                "Import backup",
                multiple = FALSE,
                accept = ".RData"
              )
            ),
            tags$br()
          )
        )
      ),

      miniTabPanel(
        "Solution",
        icon = icon("edit"),
        miniContentPanel(
          uiOutput("slctquest"),
          fillRow(
            flex = c(2, 4),
            fillCol(
              flex = c(1, 1, 9),
              actionButton(
                "updatesolution",
                "Update solution",
                style = "width:90%; background-color:#009933 !important;"
              ),
              uiOutput("question"),
              uiOutput("solution")
            ),
            fillCol(
              flex = c(1, 9),
              actionButton(
                "updatecriteria",
                "Update criteria",
                style = "width:100%; background-color:#009933 !important;"
              ),
              rhandsontable::rHandsontableOutput("criteria")
            )
          )
        )
      ),

      miniTabPanel(
        "Assess",
        icon = icon("balance-scale"),
        miniContentPanel(
          fillCol(
            flex = c(1, 9),
            fillRow(
              flex = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
              textOutput("slctsource"),
              actionButton(
                "firstsrc",
                "First",
                style = "width:100px;"
              ),
              actionButton(
                "prevsrc",
                "Previous",
                style = "width:100px;"
              ),
              actionButton(
                "reloadsrc",
                "Reload",
                style = "width:100px; background-color: #003399;"
              ),
              actionButton(
                "savesrc",
                "Save",
                style = "width:100px; background-color:#009933;"
              ),
              actionButton(
                "nextsrc",
                "Next",
                style = "width:100px;"
              ),
              actionButton(
                "lastgraded",
                "Graded",
                style = "width:100px;"
              ),
              actionButton(
                "lastsrc",
                "Last",
                style = "width:100px;"
              ),
              actionButton(
                "gotosrc",
                "Go to",
                style = "width:100px;"
              ),
              uiOutput("slctsrc")
            ),
            fillRow(
              flex = c(7, 1, 4),
              fillCol(
                flex = c(1, 9),
                uiOutput("slctcriterion"),
                uiOutput("answer")
              ),
              tags$br(),
              uiOutput("grading")
            )
          )
        )
      ),

      miniTabPanel(
        "Best-of",
        icon = icon("grin-squint-tears"),
        miniContentPanel(
          fillCol(
            flex = c(1, 11),
            actionButton(
              "updatebestof",
              "Update",
              style = "width:100%;"
            ),
            rHandsontableOutput("bestof")
          )
        )
      ),

      miniTabPanel(
        "Check",
        icon = icon("ruler"),
        miniContentPanel(
          actionButton("checkquestion", "Check/Update"),
          fillRow(
            flex = c(1, 1),
            fillCol(
              dataTableOutput("checktable")
            ),
            fillCol(
              flex = c(4, 1, 4),
              plotOutput("correlations"),
              uiOutput("select_dim"),
              plotOutput("scatterplot", brush = "slctpoint")
            )
          )
        )
      ),

      miniTabPanel(
        "Download",
        icon = icon("download"),
        miniContentPanel(
          plotOutput("distribution")
        )
      )
    )
  )


  server <- function(input, output, session) {


    # Bind variables
    answer <- NULL
    question_id <- NULL
    solution <- NULL
    criterion_scale <- NULL
    question_id <- NULL
    source_id <- NULL
    criterion_id <- NULL
    criterion_label <- NULL
    data <- NULL
    grade <- NULL
    criterion_keywords <- NULL
    criterion_order <- NULL
    name <- NULL
    increment <- NULL
    x <- NULL
    y <- NULL
    question_label <- NULL


    ############################################################################
    # Prepare reactive values

    tables <- reactiveValues()


    observeEvent(input$import, {
      if (input$creres == "Create") {

        # Download or create answers
        if (!is.null(input$answers)) {
          tables$answers <- readODS::read_ods(
            input$answers$datapath[[1]]
          ) %>%
            dplyr::mutate(
              comments = as.character(NA),
              evaluation = as.double(0.00)
            )
          tables$questions <- sort(unique(tables$answers$question_id))
          tables$sources <- unique(tables$answers$source_id)
          tables$sourceincr <- 1
          tables$lastgraded <- 1
        } else {
          tables$answers <- data.frame(
            source_id = as.character(NA),
            question_id = as.character(NA),
            format = "text",
            answer = as.character(NA),
            comments = as.character(NA),
            evaluation = as.double(0.00)
          )
          tables$questions <- c("")
          tables$sources <- c("")
          tables$sourceincr <- 1
          tables$lastgraded <- 1
        }

        # Download or create criteria
        if (!is.null(input$criteria)) {
          tables$criteria <- readODS::read_ods(
            input$criteria$datapath[[1]]
          )
        } else {
          tables$criteria <- data.frame(
            criterion_id = as.character(NA),
            criterion_order = as.numeric(NA),
            question_id = unique(tables$answers$question_id),
            criterion_label = as.character(NA),
            criterion_scale = as.character(NA),
            criterion_keywords = as.character(NA)
          )
        }

        # Download or create solutions
        if (!is.null(input$solutions)) {
          tables$solutions <- readODS::read_ods(
            input$solutions$datapath[[1]]
          )
        } else {
          tables$solutions <- data.frame(
            question_id = tables$questions,
            solution = as.character(NA),
            points = as.double(10)
          )
        }

        grading <- tables$criteria %>%
          dplyr::select(question_id, criterion_id) %>%
          dplyr::mutate(grade = 0) %>%
          group_by(question_id) %>%
          tidyr::nest()

        tables$grades <- tibble::tibble(
          source_id = tables$sources,
          question_id = list(tables$questions)
        ) %>%
          tidyr::unnest(question_id) %>%
          dplyr::left_join(grading, by = "question_id")

        tables$bestof <- tibble::tibble(
          source_id = as.numeric(NA),
          quote = as.character(NA)
        )

        tables$details <- list()
        tables$scores <- list()
      } else {
        if (!is.null(input$backup)) {
          load(input$backup$datapath)
          tables$answers <- project$answers
          tables$criteria <- project$criteria
          tables$questions <- project$questions
          tables$sources <- project$sources
          tables$sourceincr <- project$sourceincr
          tables$lastgraded <- project$lastgraded
          tables$solutions <- project$solutions
          tables$grades <- project$grades
          tables$bestof <- project$bestof
          tables$details <- project$details
          tables$scores <- project$scores
        }
      }
    })



    ############################################################################
    # Edit solutions and criteria

    output$slctquest <- renderUI({
      selectInput(
        "slctquest",
        "Select a question",
        choices = tables$questions,
        selected = tables$questions[[1]],
        width = "100%"
      )
    })

    output$question <- renderText({
      if (!is.null(tables$solutions) & !is.null(input$slctquest)) {
        textquest <- tables$solutions %>%
          dplyr::filter(
            question_id == input$slctquest
          ) %>%
          dplyr::select(question_label)
        textquest <- textquest$question_label[1]
      } else {
        textquest <- c("")
      }
      textquest
    })

    output$solution <- renderUI({
      if (!is.null(tables$solutions) & !is.null(input$slctquest)) {
        textsol <- tables$solutions %>%
          dplyr::filter(
            question_id == input$slctquest
          ) %>%
          dplyr::select(solution)
        textsol <- textsol$solution[1]
      } else {
        textsol <- c("")
      }
      textAreaInput(
        "solution",
        label = "Solution",
        value = textsol,
        height = "500px"
      ) %>%
        shiny::tagAppendAttributes(style = "width: 90%;")
    })


    observeEvent(input$updatesolution, {
      replacement <- input$solution
      new_solution <- tables$solutions %>%
        dplyr::mutate(solution = dplyr::case_when(
          question_id != input$slctquest ~ solution,
          TRUE ~ replacement
        ))
      tables$solutions <- new_solution
    })


    output$criteria <- rhandsontable::renderRHandsontable(
      if (!is.null(tables$criteria) & !is.null(input$slctquest)) {
        tables$criteria %>%
          dplyr::filter(
            question_id == input$slctquest
          ) %>%
          dplyr::mutate(
            criterion_scale = factor(
              criterion_scale,
              levels = c("logical", "qualitative", "percentage")
            )
          ) %>%
          rhandsontable::rhandsontable(
            height = 600,
            width = "100%",
            rowHeaders = NULL,
            stretchH = "all"
          ) %>%
          rhandsontable::hot_context_menu(
            allowRowEdit = TRUE,
            allowColEdit = FALSE
          )
      }
    )


    observeEvent(input$updatecriteria, {
      keep_criteria <- filter(
        tables$criteria,
        question_id != input$slctquest
      ) %>%
        dplyr::mutate(criterion_scale = as.character(criterion_scale))
      replacement <- rhandsontable::hot_to_r(input$criteria) %>%
        dplyr::mutate(criterion_scale = as.character(criterion_scale))
      new_criteria <- dplyr::bind_rows(keep_criteria, replacement) %>%
        dplyr::arrange(question_id, criterion_order)

      keep_in_grades <- tables$grades
      for (i in seq_len(nrow(keep_in_grades))) {
        keep_in_grades$data[[i]] <- dplyr::filter(
          keep_in_grades$data[[i]],
          criterion_id %in% new_criteria$criterion_id
        )
      }

      tables$criteria <- new_criteria
      tables$grades <- keep_in_grades
    })



    ############################################################################
    # Grade

    observeEvent(input$firstsrc, {
      tables$sourceincr <- 1
    })


    observeEvent(input$prevsrc, {
      tables$sourceincr <- max(1, tables$sourceincr - 1)
    })


    observeEvent(input$savesrc, {
      criteria <- tables$criteria %>%
        dplyr::filter(question_id == input$slctquest)

      keep_quest <- tables$grades %>%
        dplyr::filter(question_id != input$slctquest)

      keep_from_source <- tables$grades %>%
        dplyr::filter(
          question_id == input$slctquest,
          source_id != tables$sources[tables$sourceincr]
        )

      crit <- c()
      grad <- c()
      for (i in seq_len(nrow(criteria))) {
        inid <- as.character(criteria$criterion_id[i])
        crit <- c(crit, inid)

        if (criteria$criterion_scale[i] == "qualitative") {
          txt <- input[[inid]]
          num <- as.numeric(dplyr::case_when(
            txt == "Right" ~ 1,
            txt == "Imprecise" ~ 0.5,
            txt == "Missing" ~ 0,
            TRUE ~ -0.5
          ))
          grad <- c(grad, num)
        } else {
          num <- as.numeric(input[[inid]])
          grad <- c(grad, num)
        }
      }

      add <- tibble::tibble(
        criterion_id = crit,
        grade = grad
      )

      change_from_source <- tibble::tibble(
        question_id = input$slctquest,
        source_id = tables$sources[tables$sourceincr],
        data = list(add)
      )

      new_grades <- keep_quest %>%
        dplyr::bind_rows(keep_from_source) %>%
        dplyr::bind_rows(change_from_source)

      tables$grades <- new_grades

      new_answers <- tables$answers %>%

        dplyr::mutate(
          comments = dplyr::case_when(
            question_id != input$slctquest ~ comments,
            source_id != tables$sources[tables$sourceincr] ~ comments,
            TRUE ~ input$comments
          ),
          evaluation = dplyr::case_when(
            question_id != input$slctquest ~ evaluation,
            source_id != tables$sources[tables$sourceincr] ~ evaluation,
            TRUE ~ as.double(input$evaluation)
          )
        )
      tables$answers <- new_answers

      tables$lastgraded <- tables$sourceincr

      project <- list(
        answers = tables$answers,
        criteria = tables$criteria,
        questions = tables$questions,
        sources = tables$sources,
        sourceincr = tables$sourceincr,
        lastgraded = tables$lastgraded,
        solutions = tables$solutions,
        grades = tables$grades,
        bestof = tables$bestof,
        details = tables$details,
        scores = tables$scores
      )
      save(project, file = "project.RData")
    })


    observeEvent(input$nextsrc, {
      tables$sourceincr <- min(tables$sourceincr + 1, length(tables$sources))
    })


    observeEvent(input$lastgraded, {
      tables$sourceincr <- tables$lastgraded
    })


    observeEvent(input$lastsrc, {
      tables$sourceincr <- length(tables$sources)
    })


    output$slctsrc <- renderUI({
      numericInput(
        "goto",
        "Source",
        min = 1,
        max = length(tables$sources),
        value = tables$sourceincr
      )
    })


    observeEvent(input$gotosrc, {
      tables$sourceincr <- input$goto
    })


    output$slctsource <- renderText(
      paste0(
        "Answer number ",
        tables$sourceincr,
        " out of ",
        length(tables$sources),
        "."
      )
    )


    output$slctcriterion <- renderUI({
      if (tables$answers$format[[1]] == "video") {
        tags$br()
      } else {
        if (!is.null(input$slctquest)){
          criteria <- tables$criteria %>%
            dplyr::filter(
              question_id == input$slctquest,
              !is.null(criterion_keywords),
              !is.na(criterion_keywords),
              criterion_keywords != ""
            ) %>%
            dplyr::select(criterion_label) %>%
            unlist() %>%
            as.character()
          
          selectInput(
            "slctcrit",
            "Select keywords to highlight:",
            choices = c("None", "All", criteria),
            selected = "None",
            width = "100%"
          )
        } else tags$br()
      }
    })


    output$viewanswer <- renderUI({
      if (!is.null(tables$answers) &
        !is.null(tables$sourceincr) &
        !is.null(input$slctquest)) {
        answer <- tables$answers %>%
          dplyr::filter(
            source_id == tables$sources[tables$sourceincr],
            question_id == input$slctquest
          ) %>%
          dplyr::select(answer) %>%
          unlist() %>%
          as.character()

        if (!is.null(input$slctcrit)) {
          
          if (input$slctcrit == "None"){
            
            answer <- answer
            
          } else if (input$slctcrit == "All"){
            
            pattern <- tables$criteria %>%
              dplyr::select(criterion_keywords) %>%
              dplyr::filter(nchar(criterion_keywords) > 1) %>%
              unlist() %>%
              as.character() %>%
              stringr::str_replace_all(", ", "|") %>%
              paste(collapse = "|")
            
            pattern <- paste0(
              "(?:^|[:punct:]|[:space:])",
              pattern,
              "(?:[:punct:]|[:space:]|$)"
            )
            answer <- gsub("</span>", "</b></font>", answer)
            answer <- gsub("\n", "<br>", answer)
            answer <- stringr::str_view_all(answer, pattern, match = TRUE)
            answer <- gsub(
              "<span class='match'>",
              '<font color="red"><b>',
              answer$x$html
            )
            answer <- gsub("</span>", "</b></font>", answer)
            answer <- gsub("\n", "<br>", answer)
            
          } else {
            
            pattern <- tables$criteria %>%
              dplyr::filter(
                question_id == input$slctquest,
                criterion_label == input$slctcrit
              ) %>%
              dplyr::select(criterion_keywords) %>%
              unlist() %>%
              as.character() %>%
              stringr::str_replace_all(", ", "|")
            
            pattern <- paste0(
              "(?:^|[:punct:]|[:space:])",
              pattern,
              "(?:[:punct:]|[:space:]|$)"
            )
            answer <- stringr::str_view_all(answer, pattern, match = TRUE)
            answer <- gsub(
              "<span class='match'>",
              '<font color="red"><b>',
              answer$x$html
            )
            answer <- gsub("</span>", "</b></font>", answer)
            answer <- gsub("\n", "<br>", answer)
            
          }
          
          HTML(answer)
          
        } else {
          tags$iframe(src = answer, width = 840, height = 472)
        }
      } else {
        HTML("")
      }
    })


    output$answer <- renderUI({
      if (!is.null(tables$answers) &
        !is.null(tables$sourceincr) &
        !is.null(input$slctquest)) {
        selection <- tables$answers %>%
          dplyr::filter(
            source_id == tables$sources[tables$sourceincr],
            question_id == input$slctquest
          )

        answer <- selection %>%
          dplyr::select(answer) %>%
          unlist() %>%
          as.character()

        wordcount <- lexR::count_words(answer)

        comments <- selection %>%
          dplyr::select(comments) %>%
          unlist() %>%
          as.character()

        evaluation <- selection %>%
          dplyr::select(evaluation) %>%
          unlist() %>%
          as.numeric()

        points <- tables$solutions %>%
          dplyr::filter(question_id == input$slctquest) %>%
          dplyr::select(points) %>%
          as.numeric()

        ui <- list()
        ui[[1]] <- uiOutput("viewanswer")
        ui[[2]] <- tags$hr()
        ui[[3]] <- renderText(paste0("Word count: ", wordcount))
        ui[[4]] <- textAreaInput(
          "comments",
          "Comments:",
          value = comments,
          height = "200px"
        ) %>%
          shiny::tagAppendAttributes(style = "width: 100%;")
        ui[[5]] <- sliderInput(
          "evaluation",
          "Evaluation:",
          min = 0,
          max = points,
          step = 0.25,
          value = evaluation,
          width = "100%"
        )
        ui
      }
    })


    output$grading <- renderUI({
      input$reload

      if (!is.null(input$slctquest)) {
        criteria <- suppressWarnings(
          rhandsontable::hot_to_r(input$criteria)
        ) %>%
          dplyr::arrange(criterion_order) %>%
          dplyr::select(criterion_id, criterion_label, criterion_scale) %>%
          dplyr::mutate(criterion_scale = as.character(criterion_scale))

        prefilled <- tables$grades %>%
          dplyr::filter(
            source_id == tables$sources[tables$sourceincr],
            question_id == input$slctquest
          ) %>%
          dplyr::select(data) %>%
          tidyr::unnest(data) %>%
          dplyr::select(criterion_id, grade)

        prefilled <- criteria %>%
          dplyr::left_join(
            prefilled,
            by = "criterion_id"
          ) %>%
          tidyr::replace_na(list(grade = 0))

        ui <- list()
        for (i in seq_len(nrow(prefilled))) {
          if (prefilled$criterion_scale[i] == "logical") {
            ui[[i]] <- checkboxInput(
              prefilled$criterion_id[i],
              prefilled$criterion_label[i],
              value = prefilled$grade[i]
            )
          } else if (prefilled$criterion_scale[i] == "qualitative") {
            selected <- dplyr::case_when(
              prefilled$grade[i] == 1 ~ "Right",
              prefilled$grade[i] == 0.5 ~ "Imprecise",
              prefilled$grade[i] == 0 ~ "Missing",
              TRUE ~ "Wrong"
            )

            ui[[i]] <- radioButtons(
              prefilled$criterion_id[i],
              prefilled$criterion_label[i],
              choices = c("Wrong", "Missing", "Imprecise", "Right"),
              selected = selected,
              inline = TRUE
            )
          } else {
            ui[[i]] <- sliderInput(
              prefilled$criterion_id[i],
              prefilled$criterion_label[i],
              min = 0,
              max = 1,
              value = prefilled$grade[i],
              step = 0.25
            )
          }
        }

        ui
      }
    })


    ############################################################################
    # Best of

    output$bestof <- rhandsontable::renderRHandsontable({
      tables$bestof %>%
        rhandsontable::rhandsontable(
          height = 600,
          width = "100%",
          rowHeaders = NULL,
          stretchH = "all"
        ) %>%
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE,
          allowColEdit = FALSE
        )
    })

    observeEvent(input$updatebestof, {
      tables$bestof <- rhandsontable::hot_to_r(input$bestof)
    })

    ############################################################################
    # Checks

    # Produce metrics on demand for the current question
    observeEvent(input$checkquestion, {
      if (!is.null(input$slctquest)) {
        format <- tables$answers %>%
          dplyr::filter(question_id == input$slctquest)
        format <- format$format[[1]]

        criteria <- tables$criteria %>%
          dplyr::filter(question_id == input$slctquest)

        grades <- tables$grades %>%
          tidyr::unnest(data) %>%
          dplyr::filter(question_id == input$slctquest) %>%
          dplyr::select(-question_id) %>%
          dplyr::filter(criterion_id %in% criteria$criterion_id)

        if (nrow(criteria) > 1 & nrow(grades) > 1) {
          aggreg <- grades %>%
            dplyr::group_by(source_id) %>%
            dplyr::summarise(
              SUM = sum(grade),
              AVG = mean(grade)
            )

          discrete <- grades %>%
            dplyr::mutate(criterion_id = paste0(criterion_id, "_grade")) %>%
            tidyr::pivot_wider(
              names_from = "criterion_id", values_from = "grade"
            ) %>%
            dplyr::mutate_if(is.numeric, tidyr::replace_na, 0)

          addressed <- discrete %>%
            dplyr::mutate_if(is.numeric, function(x) as.numeric(abs(x) > 0))
          names(addressed) <- stringr::str_replace_all(
            names(addressed), "_grade", "_addressed"
          )

          options(warn = -1)

          if (nrow(discrete) > length(discrete) + 1) {

            pca <- suppressWarnings(
              suppressMessages(
                psych::pca(dplyr::select_if(discrete, is.numeric), 1)
              )
            )
            pca <- tibble::tibble(
              source_id = discrete$source_id,
              PCA = as.numeric(pca$scores)
            )

            fa <- suppressWarnings(
              suppressMessages(
                psych::fa(dplyr::select_if(discrete, is.numeric), 1)
              )
            )
            fa <- tibble::tibble(
              source_id = discrete$source_id,
              FAC = as.numeric(fa$scores)
            )
          } else {
            pca <- tibble::tibble(
              source_id = discrete$source_id,
              PCA = 0
            )

            fa <- tibble::tibble(
              source_id = discrete$source_id,
              FAC = 0
            )
          }

          options(warn = 0)

          scores <- tables$answers %>%
            dplyr::filter(question_id == input$slctquest) %>%
            dplyr::select(source_id, evaluation) %>%
            dplyr::left_join(aggreg, by = "source_id") %>%
            dplyr::left_join(pca, by = "source_id") %>%
            dplyr::left_join(fa, by = "source_id")

          if (format == "text") {
            keywords <- criteria %>%
              dplyr::select(criterion_id, criterion_keywords) %>%
              na.omit() %>%
              dplyr::filter(nchar(criterion_keywords) > 2) %>%
              dplyr::mutate(
                criterion_id = paste0(criterion_id, "_keywords"),
                criterion_keywords = purrr::map_chr(
                  criterion_keywords, stringr::str_replace_all, ", ", "|"
                )
              )

            kwcounts <- tables$answers %>%
              dplyr::filter(question_id == input$slctquest) %>%
              dplyr::select(source_id, answer) %>%
              dplyr::mutate(keywords = list(keywords)) %>%
              tidyr::unnest(keywords) %>%
              dplyr::mutate(count = purrr::map2_int(
                answer, criterion_keywords, stringr::str_count
              )) %>%
              dplyr::select(source_id, criterion_id, count) %>%
              tidyr::pivot_wider(names_from = criterion_id, values_from = count)

            details <- discrete %>%
              dplyr::left_join(addressed, by = "source_id") %>%
              dplyr::left_join(kwcounts, by = "source_id") %>%
              tidyr::pivot_longer(cols = !dplyr::matches("source_id")) %>%
              tidyr::separate(
                name,
                into = c("criterion_id", "type"), sep = "_"
              ) %>%
              tidyr::pivot_wider(
                names_from = "type", values_from = "value", values_fill = NA
              )

            add2score <- details %>%
              dplyr::group_by(source_id) %>%
              dplyr::summarize(KWD = sum(keywords, na.rm = TRUE))

            scores <- scores %>%
              dplyr::left_join(add2score, by = "source_id")
          } else {
            scores <- scores %>%
              dplyr::mutate(KWD = 0)
          }

          tables$details[[input$slctquest]] <- details %>%
            dplyr::mutate(question_id = input$slctquest) %>%
            dplyr::select(source_id, question_id, dplyr::everything())

          tables$scores[[input$slctquest]] <- scores %>%
            dplyr::mutate(question_id = input$slctquest) %>%
            dplyr::select(source_id, question_id, dplyr::everything())
        }
      }
    })

    # Display graphs and tables

    basescatterplot <- reactive({
      if (!is.null(input$slctquest)) {
        if (!is.null(tables$scores[[input$slctquest]])) {
          increments <- tibble::tibble(
            source_id = tables$sources,
            increment = seq_len(length(tables$sources))
          )
          tables$scores[[input$slctquest]] %>%
            dplyr::select(source_id, x = input$slctx, y = input$slcty) %>%
            dplyr::left_join(increments, by = "source_id")
        }
      }
    })

    output$checktable <- renderDataTable({
      if (!is.null(input$slctquest)) {
        if (!is.null(tables$details[[input$slctquest]])) {
          tables$details[[input$slctquest]] %>%
            dplyr::left_join(tables$criteria, by = "criterion_id") %>%
            dplyr::left_join(basescatterplot(), by = "source_id") %>%
            dplyr::select(
              source = increment, criterion_label, addressed, keywords
            ) %>%
            na.omit() %>%
            dplyr::filter(
              (addressed == 0 & keywords > 0) |
                (addressed > 0 & keywords == 0)
            ) %>%
            dplyr::arrange(addressed, -keywords)
        }
      }
    })


    output$correlations <- renderPlot({
      if (!is.null(input$slctquest)) {
        if (!is.null(tables$scores[[input$slctquest]])) {
          psych::pairs.panels(
            dplyr::select_if(
              tables$scores[[input$slctquest]],
              is.numeric
            )
          )
        }
      }
    })


    output$select_dim <- renderUI({
      if (!is.null(input$slctquest)) {
        if (!is.null(tables$scores[[input$slctquest]])) {
          choices <- c("evaluation", "SUM", "AVG", "PCA", "FAC", "KWD")
          ui <- list(
            fillRow(
              flex = c(1, 1),
              selectInput(
                "slctx",
                "Select x-axis",
                choices = choices,
                selected = "SUM"
              ),
              selectInput(
                "slcty",
                "Select y-axis",
                choices = choices,
                selected = "evaluation"
              )
            )
          )
          ui
        }
      }
    })

    output$scatterplot <- renderPlot({
      if (!is.null(input$slctx) &
        !is.null(input$slcty) &
        !is.null(basescatterplot())) {
        basescatterplot() %>%
          na.omit() %>%
          ggplot2::ggplot(ggplot2::aes(
            x = x, y = y, label = increment
          )) +
          ggplot2::geom_label() +
          ggplot2::geom_smooth(method = "lm", formula = y ~ x) +
          ggplot2::xlab(input$slctx) +
          ggplot2::ylab(input$slcty)
      }
    })


    observeEvent(input$slctpoint, {
      if (!is.null(basescatterplot())) {
        selection <- basescatterplot() %>%
          dplyr::filter(
            x <= input$slctpoint$xmax,
            x >= input$slctpoint$xmin,
            y <= input$slctpoint$ymax,
            y >= input$slctpoint$ymin
          )

        if (nrow(selection) > 0) tables$sourceincr <- selection$increment[[1]]
      }
    })


    ############################################################################
    # Export

    # Grade distribution
    output$distribution <- renderPlot({
      tables$answers %>%
        na.omit() %>%
        dplyr::group_by(source_id) %>%
        dplyr::summarize(evaluation = sum(evaluation)) %>%
        dplyr::ungroup() %>%
        dplyr::count(evaluation) %>%
        ggplot2::ggplot(ggplot2::aes(x = evaluation, y = n)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::xlab("Grade") +
        ggplot2::ylab("Count")
    })


    # Download


    ############################################################################
    # On exit

    observeEvent(input$done, {
      
      readODS::write_ods(tables$criteria, "criteria_out.ods")
      readODS::write_ods(tables$solutions, "solutions_out.ods")
      readODS::write_ods(
        dplyr::mutate(
          tidyr::unnest(tables$grades, data),
          weight = 1
        ),
        "grades_out.ods"
      )
      readODS::write_ods(tables$bestof, "bestof_out.ods")
      
      project <- list(
        answers = tables$answers,
        criteria = tables$criteria,
        questions = tables$questions,
        sources = tables$sources,
        sourceincr = tables$sourceincr,
        lastgraded = tables$lastgraded,
        solutions = tables$solutions,
        grades = tables$grades,
        bestof = tables$bestof,
        details = tables$details,
        scores = tables$scores
      )
      save(project, file = "project.RData")

      stopApp()
    })
  }

  runGadget(ui, server, viewer = shiny::browserViewer())
}
