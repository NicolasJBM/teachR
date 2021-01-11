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
#' @importFrom readxl read_excel
#' @importFrom lexR count_words
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
                  accept = c(".xlsx")
                ),
                fileInput(
                  "criteria",
                  "Import criteria",
                  multiple = FALSE,
                  accept = c(".csv", ".xlsx")
                ),
                fileInput(
                  "solutions",
                  "Import solutions",
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
            flex = c(2, 3),
            fillCol(
              flex = c(4, 1),
              uiOutput("solution"),
              actionButton(
                "updatesolution",
                "Update solution"
              )
            ),
            fillCol(
              flex = c(4, 1),
              rhandsontable::rHandsontableOutput("criteria"),
              actionButton(
                "updatecriteria",
                "Update criteria"
              )
            )
          )
        )
      ),

      miniTabPanel(
        "Grade",
        icon = icon("check"),
        miniContentPanel(
          fillCol(
            flex = c(1,9),
            fillRow(
              flex = c(1, 1, 1, 1, 1),
              textOutput("slctsource"),
              actionButton(
                "backstart",
                "First source",
                style = "width:150px;"
              ),
              actionButton(
                "prevstud",
                "Previous source",
                style = "width:150px;"
              ),
              actionButton(
                "nextstud",
                "Next source",
                style = "width:150px;"
              ),
              actionButton(
                "lastgraded",
                "Last graded",
                style = "width:150px;"
              )
            ),
            fillRow(
              flex = c(5,1,4),
              uiOutput("answer"),
              tags$br(),
              uiOutput("grading")
            )
          )
        )
      ),

      miniTabPanel(
        "Download",
        icon = icon("download"),
        miniContentPanel()
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

    ############################################################################
    # Prepare reactive values

    tables <- reactiveValues()
    
    observeEvent(input$import, {
      if (input$creres == "Create") {

        # Download or create answers
        if (!is.null(input$answers)) {
          tables$answers <- readxl::read_excel(
            input$answers$datapath[[1]]) %>%
            dplyr::mutate(
              comments = as.character(NA),
              evaluation = as.numeric(0)
            )
          tables$questions <- sort(unique(tables$answers$question_id))
          tables$sources <- unique(tables$answers$source_id)
          tables$sourceincr <- 1
          tables$lastgraded <- 1
        } else {
          tables$answers <- data.frame(
            source_id = as.character(NA),
            source_type = as.character(NA),
            question_id = as.character(NA),
            answer = as.character(NA),
            comments = as.character(NA),
            evaluation = as.numeric(0)
          )
          tables$questions <- c("")
          tables$sources <- c("")
          tables$sourceincr <- 1
          tables$lastgraded <- 1
        }

        # Download or create criteria
        if (!is.null(input$criteria)) {
          tables$criteria <- readxl::read_excel(
            input$criteria$datapath[[1]]) %>%
            dplyr::mutate(
              criterion_scale = factor(
                criterion_scale,
                levels = c("presence", "understanding", "intensity")
              )
            )
        } else {
          tables$criteria <- data.frame(
            question_id = unique(tables$answers$question_id),
            criterion_id = as.character(NA),
            criterion_nbr = as.numeric(NA),
            criterion_label = as.character(NA),
            criterion_scale = factor(
              NA,
              levels = c("presence", "understanding", "intensity")
            )
          )
        }
        
        # Download or create solutions
        if (!is.null(input$solutions)) {
          tables$solutions <- readxl::read_excel(
            input$solutions$datapath[[1]])
        } else {
          tables$solutions <- data.frame(
            question_id = tables$questions,
            solution = as.character(NA)
          )
        }
        
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
        } 
      }

      grading <- tables$criteria %>%
        dplyr::mutate(grade = 0) %>%
        group_by(question_id) %>%
        tidyr::nest()
      
      tables$grades <- tibble::tibble(
        source_id = tables$sources,
        question_id = list(tables$questions)
      ) %>%
        tidyr::unnest(question_id) %>%
        dplyr::left_join(grading, by = "question_id")
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
        shiny::tagAppendAttributes(style = 'width: 90%;')
    })

    output$criteria <- rhandsontable::renderRHandsontable(
      if (!is.null(tables$criteria) & !is.null(input$slctquest)) {
        tables$criteria %>%
          dplyr::filter(
            question_id == input$slctquest
          ) %>%
          rhandsontable::rhandsontable(
            height = 500,
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

    ############################################################################
    # Grade
    observeEvent(input$backstart, {
      tables$sourceincr <- 1
    })

    observeEvent(input$prevstud, {
      tables$sourceincr <- max(1, tables$sourceincr - 1)
    })

    observeEvent(input$nextstud, {
      tables$sourceincr <- min(tables$sourceincr + 1, length(tables$sources))
    })

    observeEvent(input$lastgraded, {
      tables$sourceincr <- tables$lastgraded
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
        
        ui <- list()
        ui[[1]] <- renderText(answer)
        ui[[2]] <- tags$hr()
        ui[[3]] <- renderText(paste0("Word count: ", wordcount))
        ui[[4]] <- textAreaInput(
          "comments",
          'Comments:',
          value = comments,
          height = "200px"
        ) %>%
          shiny::tagAppendAttributes(style = 'width: 90%;')
        ui[[5]] <- numericInput(
          "evaluation",
          "Evaluation:",
          min = 0,
          max = 10,
          step = 0.25,
          value = evaluation
        )
        ui
      }
    })
    
    output$grading <- renderUI({
      
      criteria <- suppressWarnings(
        rhandsontable::hot_to_r(input$criteria)
      ) %>%
        dplyr::mutate(criterion_scale = as.character(criterion_scale))
      
      prefilled <- tables$grades %>%
        dplyr::filter(
          source_id == tables$sources[tables$sourceincr],
          question_id == input$slctquest
        ) %>%
        dplyr::select(question_id, data) %>%
        tidyr::unnest(data) %>%
        dplyr::select(-criterion_label, -criterion_scale) %>%
        dplyr::mutate(criterion_scale = as.character(criterion_scale)) %>%
        dplyr::full_join(
          criteria,
          by = c(
            "question_id",
            "criterion_id",
            "criterion_nbr"
          )
        ) %>%
        tidyr::replace_na(list(grade = 0))
      
      ui <- list()
      for (i in 1:nrow(prefilled)){
        
        if (prefilled$criterion_scale[i] == "presence"){
          ui[[i]] <- checkboxInput(
            prefilled$criterion_id[i],
            prefilled$criterion_label[i],
            value = prefilled$grade[i]
          )
        } else if(prefilled$criterion_scale[i] == "understanding"){
          
          selected <- dplyr::case_when(
            prefilled$grade[i] == 2 ~ "Right",
            prefilled$grade[i] == 1 ~ "Imprecise",
            prefilled$grade[i] == 0 ~ "Missing",
            TRUE ~ "Wrong"
          )
          
          ui[[i]] <- radioButtons (
            prefilled$criterion_id[i],
            prefilled$criterion_label[i],
            choices = c("Right","Imprecise","Missing","Wrong"),
            selected = selected,
            inline = TRUE
          )
          
        } else {
          ui[[i]] <- sliderInput(
            prefilled$criterion_id[i],
            prefilled$criterion_label[i],
            min = -2,
            max = 2,
            value = prefilled$grade[i],
            step = 1
          )
        }
      }
      
      ui
    })
    
    ############################################################################
    # Update structure and grades
    
    

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
        grades = tables$grades
      )
      
      save(project, file = "project.RData")
      
      stopApp()
    })
  }

  runGadget(ui, server, viewer = shiny::browserViewer())
}
