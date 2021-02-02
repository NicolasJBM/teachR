#' @name test_grade
#' @title Grade open-ended answers
#' @author Nicolas Mangin
#' @description Shiny gadget to facilitate grading open-ended questions and essays.
#' @return Three .csv files: one with students' points, one with updated solutions, one with updated criteria.
#' @import miniUI
#' @import shiny
#' @importFrom bslib bs_theme
#' @importFrom bslib font_google
#' @importFrom readxl read_xlsx
#' @importFrom WriteXLS WriteXLS
#' @importFrom dplyr count
#' @importFrom lexR count_words
#' @importFrom lexR clean_ascii
#' @importFrom purrr map_chr
#' @importFrom dplyr mutate_if
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom psych pca
#' @importFrom psych fa
#' @importFrom stats sd
#' @export



test_grade <- function() {
  ui <- miniPage(
    theme = bslib::bs_theme(
      bootswatch = "flatly",
      base_font = bslib::font_google("Open Sans"),
      "enable-gradients" = FALSE,
      "enable-shadows" = TRUE,
      spacer = "0.5rem"
    ),

    tags$head(tags$style(
      HTML(".shiny-notification {
              position:fixed;top: 30%;left: 0%;right: 0%;
           }")
    )),

    gadgetTitleBar("Grade open-ended answers"),
    miniTabstripPanel(
      miniTabPanel(
        "Import",
        icon = icon("upload"),
        miniContentPanel(
          fillRow(
            flex = c(1, 1, 1, 1),
            height = "400px",
            radioButtons(
              "creres",
              "Create or resume a project",
              choices = c("Create", "Resume"),
              selected = " Create",
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.creres === 'Create'",
              fillCol(
                flex = c(1, 1, 1, 1),
                height = "400px",
                fileInput(
                  "answers",
                  "Import answers",
                  multiple = FALSE,
                  accept = c(".xlsx")
                ),
                fileInput(
                  "criteria",
                  "Import criteria",
                  multiple = FALSE,
                  accept = c(".xlsx")
                ),
                fileInput(
                  "solutions",
                  "Import solutions",
                  multiple = FALSE,
                  accept = c(".xlsx")
                ),
                fileInput(
                  "groups",
                  "Import groups",
                  multiple = FALSE,
                  accept = c(".xlsx")
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
            actionButton(
              "import",
              "Import",
              style = "width:150px; background-color:#990033; color: #FFF;",
              icon = icon("upload")
            )
          )
        )
      ),

      miniTabPanel(
        "Solution",
        icon = icon("edit"),
        miniContentPanel(
          fillCol(
            flex = c(1, 1, 8),
            uiOutput("slctquest"),
            tags$br(),
            fillRow(
              flex = c(5, 7),
              fillCol(
                flex = c(1, 2, 5),
                actionButton(
                  "updatesolution",
                  "Update solution",
                  style = "width:90%; background-color:#009933; color: #FFF;",
                  icon = icon("save")
                ),
                uiOutput("question"),
                uiOutput("solution")
              ),
              fillCol(
                flex = c(1, 7),
                actionButton(
                  "updatecriteria",
                  "Update criteria",
                  style = "width:100%; background-color:#009933; color: #FFF;",
                  icon = icon("save")
                ),
                rhandsontable::rHandsontableOutput("criteria")
              )
            )
          )
        )
      ),

      miniTabPanel(
        "Grade",
        icon = icon("sliders-h"),
        miniContentPanel(
          fillCol(
            flex = c(1, 9),
            fillRow(
              flex = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
              textOutput("slctsource"),
              actionButton(
                "firstsrc",
                "First",
                icon = icon("angle-double-left"),
                style = "width:100px; background-color:#990033; color: #FFF;"
              ),
              actionButton(
                "prevsrc",
                "Previous",
                icon = icon("angle-left"),
                style = "width:100px; background-color:#660099; color: #FFF;"
              ),
              actionButton(
                "reloadsrc",
                "Reload",
                icon = icon("redo"),
                style = "width:100px; background-color: #000066; color: #FFF;"
              ),
              actionButton(
                "savesrc",
                "Save",
                icon = icon("save"),
                style = "width:100px; background-color:#009933; color: #FFF;"
              ),
              actionButton(
                "lastgraded",
                "Graded",
                icon = icon("edit"),
                style = "width:100px; background-color:#999900; color: #FFF;"
              ),
              actionButton(
                "nextsrc",
                "Next",
                icon = icon("angle-right"),
                style = "width:100px; background-color:#996600; color: #FFF;"
              ),
              actionButton(
                "lastsrc",
                "Last",
                icon = icon("angle-double-right"),
                style = "width:100px; background-color:#993300; color: #FFF;"
              ),
              actionButton(
                "gotosrc",
                "Go to",
                icon = icon("chevron-circle-right"),
                style = "width:100px; background-color:#006666; color: #FFF;"
              ),
              uiOutput("slctsrc")
            ),
            fillRow(
              flex = c(7, 5),
              fillCol(
                flex = c(1, 11),
                uiOutput("slctcriterion"),
                uiOutput("answer")
              ),
              uiOutput("grading")
            )
          )
        )
      ),

      miniTabPanel(
        "Weights",
        icon = icon("balance-scale"),
        miniContentPanel(
          fillCol(
            flex = c(1, 8),
            actionButton(
              "runregr",
              "Compute",
              icon = icon("redo"),
              style = "width:100%; background-color: #006699; color: #FFF;"
            ),
            fillRow(
              flex = c(1, 1),
              plotOutput("coefficients", height = "100%"),
              fillCol(
                flex = c(7,1),
                plotOutput("residuals", brush = "slctpoint", height = "100%"),
                fillRow(
                  flex = c(1,1),
                  actionButton(
                    "addresiduals",
                    "Add residuals",
                    icon = icon("save"),
                    style = "width:100%; background-color:#009933; color: #FFF;"
                  ),
                  actionButton(
                    "rmresiduals",
                    "Remove residuals",
                    icon = icon("undo"),
                    style = "width:100%; background-color:#990000; color: #FFF;"
                  )
                )
              )
            )
          )
        )
      ),

      miniTabPanel(
        "Check",
        icon = icon("ruler"),
        miniContentPanel(
          fillRow(
            flex = c(3, 2),
            fillCol(
              dataTableOutput("checktable")
            ),
            fillCol(
              flex = c(1, 7),
              actionButton(
                "checkquestion",
                "Update",
                icon = icon("redo"),
                style = "width:100%; background-color: #006699; color: #FFF;"
              ),
              plotOutput("correlations", height = "100%")
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
              "Save",
              icon = icon("save"),
              style = "width:100%; background-color:#009933; color: #FFF;"
            ),
            rHandsontableOutput("bestof")
          )
        )
      ),

      miniTabPanel(
        "Export",
        icon = icon("download"),
        miniContentPanel(
          fillCol(
            flex = c(4, 1, 1),
            plotOutput("distribution"),
            textInput(
              "part",
              "Name of the part: ",
              value = "part",
              width = "100%"
            ),
            actionButton(
              "exportxlsx",
              "Export",
              icon = icon("download"),
              style = "width:100%; background-color:#009933; color: #FFF;"
            )
          )
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
    student_id <- NULL
    criterion_id <- NULL
    criterion_label <- NULL
    data <- NULL
    grade <- NULL
    criterion_keywords <- NULL
    criterion_order <- NULL
    name <- NULL
    increment <- NULL
    question_label <- NULL
    part <- NULL
    type <- NULL
    divider <- NULL
    weight <- NULL
    FIT <- NULL


    ############################################################################
    # Prepare reactive values

    tables <- reactiveValues()


    observeEvent(input$import, {
      if (input$creres == "Create") {

        # Download or create answers
        if (!is.null(input$answers)) {
          answers <- readxl::read_excel(
            input$answers$datapath[[1]]
          )
          tables$answers <- answers %>%
            tidyr::pivot_wider(
              names_from = "question_id",
              values_from = "answer",
              values_fill = ""
            ) %>%
            tidyr::pivot_longer(
              cols = unique(answers$question_id),
              names_to = "question_id",
              values_to = "answer"
            ) %>%
            tidyr::replace_na(list(answer = "")) %>%
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
          tables$criteria <- readxl::read_excel(
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
          tables$solutions <- readxl::read_excel(
            input$solutions$datapath[[1]]
          )
        } else {
          tables$solutions <- data.frame(
            question_id = tables$questions,
            solution = as.character(NA),
            points = as.double(10)
          )
        }

        # Download or create groups
        if (!is.null(input$groups)) {
          tables$groups <- readxl::read_excel(
            input$groups$datapath[[1]]
          )
        } else {
          tables$groups <- data.frame(
            student_id = tables$sources,
            source_id = tables$sources
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
        tables$coefficients <- list()
        tables$residuals <- list()
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
          tables$groups <- project$groups
          tables$bestof <- project$bestof
          tables$details <- project$details
          tables$scores <- project$scores
          tables$coefficients <- project$coefficients
          tables$residuals <- project$residuals
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

    output$question <- renderUI({
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
      column(HTML(textquest), width = 11)
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
        height = "100px"
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
              levels = c("logical", "qualitative", "percentage", "adjustment")
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
        groups = tables$groups,
        bestof = tables$bestof,
        details = tables$details,
        scores = tables$scores,
        coefficients = tables$coefficients,
        residuals = tables$residuals
      )
      save(project, file = "project.RData")
    })


    observeEvent(input$lastgraded, {
      tables$sourceincr <- tables$lastgraded
    })


    observeEvent(input$nextsrc, {
      tables$sourceincr <- min(tables$sourceincr + 1, length(tables$sources))
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
        if (!is.null(input$slctquest)) {
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
            width = "90%"
          )
        } else {
          tags$br()
        }
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
          if (input$slctcrit == "None") {
            answer <- answer
          } else if (input$slctcrit == "All") {
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
        ui[[1]] <- tags$hr()
        ui[[2]] <- uiOutput("viewanswer")
        ui[[3]] <- tags$hr()
        ui[[4]] <- renderText(paste0("Word count: ", wordcount))
        ui[[5]] <- sliderInput(
          "evaluation",
          "Evaluation:",
          min = 0,
          max = points,
          step = 0.25,
          value = evaluation,
          width = "100%"
        )
        ui[[6]] <- textAreaInput(
          "comments",
          "Comments:",
          value = comments,
          height = "100px"
        ) %>%
          shiny::tagAppendAttributes(style = "width: 100%;")

        column(ui, width = 11)
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
              value = prefilled$grade[i],
              width = "100%"
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
              inline = TRUE,
              width = "100%"
            )
          } else if (prefilled$criterion_scale[i] == "percentage") {
            ui[[i]] <- sliderInput(
              prefilled$criterion_id[i],
              prefilled$criterion_label[i],
              min = 0,
              max = 1,
              value = prefilled$grade[i],
              step = 0.25,
              width = "100%"
            )
          } else {
            ui[[i]] <- sliderInput(
              prefilled$criterion_id[i],
              prefilled$criterion_label[i],
              min = (min(prefilled$grade)-0.5),
              max = (max(prefilled$grade)+0.5),
              value = prefilled$grade[i],
              step = 0.25,
              width = "100%"
            )
          }
        }

        ui
      }
    })


    ############################################################################
    # Weights
    observeEvent(input$runregr, {
      dependent <- tables$answers %>%
        dplyr::filter(question_id == input$slctquest) %>%
        dplyr::select(source_id, evaluation) %>%
        tidyr::replace_na(list(evaluation = 0))

      independent <- tables$grades %>%
        dplyr::filter(question_id == input$slctquest) %>%
        tidyr::unnest(data) %>%
        dplyr::select(source_id, criterion_id, grade) %>%
        tidyr::pivot_wider(
          names_from = criterion_id,
          values_from = grade,
          values_fill = 0
        )

      baseregr <- independent %>%
        dplyr::left_join(dependent, by = "source_id") %>%
        tibble::column_to_rownames("source_id")

      baseregr <- baseregr[, (apply(baseregr, 2, sd) != 0)]
      regression <- stats::lm(evaluation ~ ., data = baseregr)

      coefficients <- as.data.frame(regression$coefficients) %>%
        tibble::rownames_to_column("criterion_id") %>%
        dplyr::filter(criterion_id != "(Intercept)")
      names(coefficients) <- c("criterion_id", "coefficients")
      coefficients <- coefficients %>%
        dplyr::left_join(
          dplyr::select(
            tables$criteria,
            criterion_id, criterion_order, criterion_label
          ),
          by = "criterion_id"
        )

      increments <- tibble::tibble(
        source_id = tables$sources,
        increment = seq_len(length(tables$sources))
      )

      residuals <- as.data.frame(regression$residuals) %>%
        tibble::rownames_to_column("source_id") %>%
        dplyr::left_join(dependent, by = "source_id") %>%
        dplyr::left_join(increments, by = "source_id")
      names(residuals) <- c("source_id", "residuals", "evaluation", "increment")

      tables$coefficients[[input$slctquest]] <- coefficients %>%
        dplyr::mutate(question_id = input$slctquest) %>%
        dplyr::select(source_id, question_id, dplyr::everything())

      tables$residuals[[input$slctquest]] <- residuals %>%
        dplyr::mutate(question_id = input$slctquest) %>%
        dplyr::select(source_id, question_id, dplyr::everything())
    })

    output$coefficients <- renderPlot({
      if (!is.null(input$slctquest)) {
        if (length(tables$coefficients[[input$slctquest]]) > 1) {
          basecoeff <- tables$coefficients[[input$slctquest]] %>%
            na.omit() %>%
            dplyr::arrange(-criterion_order)
          basecoeff$criterion_label <- factor(
            basecoeff$criterion_label,
            levels = basecoeff$criterion_label
          )
          basecoeff %>%
            ggplot2::ggplot(ggplot2::aes(
              x = criterion_label,
              y = coefficients
            )) +
            ggplot2::geom_bar(stat = "identity") +
            ggplot2::xlab("Criteria") +
            ggplot2::ylab("Weight") +
            ggplot2::coord_flip()
        }
      }
    })

    output$residuals <- renderPlot({
      if (!is.null(input$slctquest)) {
        if (length(tables$residuals[[input$slctquest]]) > 1) {
          basresid <- tables$residuals[[input$slctquest]]

          basresid %>%
            na.omit() %>%
            ggplot2::ggplot(ggplot2::aes(
              x = evaluation,
              y = residuals,
              label = increment
            )) +
            ggplot2::geom_label() +
            ggplot2::scale_x_continuous(
              limits = c(0, max(basresid$evaluation)),
              breaks = seq(0, max(basresid$evaluation), by = 1)
            ) +
            ggplot2::geom_hline(yintercept = 0) +
            ggplot2::annotate("text", x = 0, y = 1, label = "with bonus") +
            ggplot2::annotate("text", x = 0, y = -1, label = "with penalty")
        }
      }
    })

    observeEvent(input$slctpoint, {
      if (!is.null(input$slctquest)) {
        if (length(tables$residuals[[input$slctquest]]) > 1) {
          selection <- tables$residuals[[input$slctquest]] %>%
            dplyr::filter(
              evaluation <= input$slctpoint$xmax,
              evaluation >= input$slctpoint$xmin,
              residuals <= input$slctpoint$ymax,
              residuals >= input$slctpoint$ymin
            )
          if (nrow(selection) > 0) {
            tables$sourceincr <- selection$increment[[1]]
          }
        }
      }
    })
    
    
    observeEvent(input$addresiduals, {
      if (!is.null(input$slctquest)){
        delta_label <- paste0(input$slctquest, "_delta")
        
        residuals <- tables$residuals[[input$slctquest]] %>%
          dplyr::select(source_id, question_id, grade = residuals) %>%
          dplyr::mutate(grade = round(grade*4,0)/4) %>%
          dplyr::mutate(criterion_id = delta_label)
        
        add_criterion <- data.frame(
          criterion_id = delta_label,
          criterion_order = 0,
          question_id = input$slctquest,
          criterion_label = "Manual adjustment",
          criterion_scale = "adjustment",
          criterion_keywords = as.character(NA)
        )
        
        criteria <- tables$criteria %>%
          dplyr::filter(criterion_id != delta_label) %>%
          dplyr::bind_rows(add_criterion)
        
        grades <- tables$grades %>%
          tidyr::unnest(data) %>%
          dplyr::filter(criterion_id != delta_label) %>%
          dplyr::bind_rows(residuals) %>%
          dplyr::group_by(source_id, question_id) %>%
          tidyr::nest() %>%
          dplyr::ungroup()
        
        tables$criteria <- criteria
        tables$grades <- grades
      }
    })
    
    observeEvent(input$rmresiduals, {
      if (!is.null(input$slctquest)){
        delta_label <- paste0(input$slctquest, "_delta")
        
        criteria <- tables$criteria %>%
          dplyr::filter(criterion_id != delta_label)
        
        grades <- tables$grades %>%
          tidyr::unnest(data) %>%
          dplyr::filter(criterion_id != delta_label) %>%
          dplyr::group_by(source_id, question_id) %>%
          tidyr::nest() %>%
          dplyr::ungroup()
        
        tables$criteria <- criteria
        tables$grades <- grades
      }
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
          dplyr::filter(
            question_id == input$slctquest,
            criterion_id != paste0(input$slctquest, "_delta")
          )

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
                psych::pca(
                  dplyr::select_if(
                    dplyr::select_if(
                      discrete, is.numeric
                    ),
                    function(x) sd(x) != 0
                  ),
                  1
                )
              )
            )
            pca <- tibble::tibble(
              source_id = discrete$source_id,
              PCA = as.numeric(pca$scores)
            )

            fa <- suppressWarnings(
              suppressMessages(
                psych::fa(
                  dplyr::select_if(
                    dplyr::select_if(
                      discrete, is.numeric
                    ),
                    function(x) sd(x) != 0
                  ),
                  1
                )
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

          fit <- grades %>%
            dplyr::left_join(
              tables$coefficients[[input$slctquest]],
              by = "criterion_id"
            ) %>%
            dplyr::mutate(FIT = grade * coefficients) %>%
            dplyr::group_by(source_id) %>%
            dplyr::summarise(FIT = sum(FIT, na.rm = TRUE)) %>%
            dplyr::ungroup()

          scores <- tables$answers %>%
            dplyr::filter(question_id == input$slctquest) %>%
            dplyr::select(source_id, evaluation) %>%
            dplyr::left_join(fit, by = "source_id") %>%
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
              tidyr::pivot_wider(
                names_from = criterion_id,
                values_from = count,
                values_fill = 0
              )

            details <- discrete %>%
              dplyr::left_join(addressed, by = "source_id") %>%
              dplyr::left_join(kwcounts, by = "source_id") %>%
              tidyr::pivot_longer(cols = !dplyr::matches("source_id")) %>%
              tidyr::separate(
                name,
                into = c("criterion_id", "type"), sep = "_"
              ) %>%
              tidyr::pivot_wider(
                names_from = "type",
                values_from = "value",
                values_fill = 0
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

    basecheck <- reactive({
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

    output$checktable <- renderDataTable(
      {
        if (!is.null(input$slctquest)) {
          if (!is.null(tables$details[[input$slctquest]])) {
            tables$details[[input$slctquest]] %>%
              dplyr::left_join(tables$criteria, by = "criterion_id") %>%
              dplyr::left_join(basecheck(), by = "source_id") %>%
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
      },
      options = list(pageLength = 8)
    )


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
    # Export

    # Grade distribution
    output$distribution <- renderPlot({
      basedistrib <- tables$answers %>%
        na.omit() %>%
        dplyr::group_by(source_id) %>%
        dplyr::summarize(evaluation = sum(evaluation)) %>%
        dplyr::ungroup() %>%
        dplyr::count(evaluation)

      basedistrib %>%
        ggplot2::ggplot(ggplot2::aes(x = evaluation, y = n)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::xlab("Grade") +
        ggplot2::ylab("Count") +
        ggplot2::scale_x_continuous(
          limits = c(-1, (max(basedistrib$evaluation) + 1)),
          breaks = seq(-1, (max(basedistrib$evaluation) + 1), by = 1)
        )
    })


    # Download
    observeEvent(input$exportxlsx, {
      solutions <- tables$solutions %>%
        dplyr::mutate(type = "text", part = input$part) %>%
        dplyr::mutate_if(
          is.character,
          function(x) purrr::map_chr(x, lexR::clean_ascii)
        ) %>%
        dplyr::select(
          question_id, question_label, solution, points
        )

      criteria <- tables$criteria %>%
        dplyr::mutate(type = "text", part = input$part) %>%
        dplyr::left_join(
          dplyr::select(
            bind_rows(tables$coefficients),
            criterion_id,
            weight = coefficients
          ),
          by = "criterion_id"
        ) %>%
        dplyr::left_join(
          dplyr::select(solutions, question_id, points),
          by = "question_id"
        ) %>%
        dplyr::mutate_if(
          is.character,
          function(x) purrr::map_chr(x, lexR::clean_ascii)
        ) %>%
        dplyr::group_by(question_id) %>%
        dplyr::mutate(divider = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(points = points / divider) %>%
        dplyr::select(
          part, question_id, criterion_id,
          criterion_order, criterion_label,
          weight, points
        )

      groups <- tables$groups %>%
        dplyr::group_by(source_id) %>%
        tidyr::nest()

      grades <- tables$grades %>%
        tidyr::unnest(data) %>%
        dplyr::mutate(type = "text", part = input$part) %>%
        dplyr::left_join(groups, by = "source_id") %>%
        tidyr::unnest(data) %>%
        dplyr::select(
          student_id, part, question_id, criterion_id, type, grade
        ) %>%
        dplyr::mutate_if(
          is.character,
          function(x) purrr::map_chr(x, lexR::clean_ascii)
        )
      bestof <- tables$bestof %>%
        dplyr::mutate_if(
          is.character,
          function(x) purrr::map_chr(x, lexR::clean_ascii)
        )

      WriteXLS::WriteXLS(
        criteria,
        paste0("criteria_", input$part, ".xlsx")
      )
      WriteXLS::WriteXLS(
        solutions,
        paste0("solutions_", input$part, ".xlsx")
      )
      WriteXLS::WriteXLS(
        grades,
        paste0("grades_", input$part, ".xlsx")
      )
      WriteXLS::WriteXLS(
        bestof,
        paste0("bestof_", input$part, ".xlsx")
      )
    })


    ############################################################################
    # On exit

    observeEvent(input$done, {
      project <- list(
        answers = tables$answers,
        criteria = tables$criteria,
        questions = tables$questions,
        sources = tables$sources,
        sourceincr = tables$sourceincr,
        lastgraded = tables$lastgraded,
        solutions = tables$solutions,
        grades = tables$grades,
        groups = tables$groups,
        bestof = tables$bestof,
        details = tables$details,
        scores = tables$scores,
        coefficients = tables$coefficients,
        residuals = tables$residuals
      )
      save(project, file = "project.RData")

      stopApp()
    })
  }

  runGadget(ui, server, viewer = shiny::browserViewer())
}
