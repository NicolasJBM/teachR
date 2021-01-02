#' Shiny gadget to facilitate grading open questions and essays.
#' @return Three .csv files: one with students' points, one with updated solutions, one with updated criterias.
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
#' @export


gradEssay <- function() {
  ui <- miniPage(
    theme = shinythemes::shinytheme("flatly"),
    tags$head(tags$style(
      HTML(".shiny-notification {
              position:fixed;top: 30%;left: 0%;right: 0%;
           }")
    )),
    
    gadgetTitleBar("Grade open questions and essays"),
    fillCol(
      flex = c(2,1,6,1,1,1,6,1),
      fillRow(
        flex = c(1,1,1,1),
        fileInput("answers", "Import answers", multiple = FALSE, accept = ".csv"),
        fileInput("solutions", "Import solutions", multiple = FALSE, accept = ".csv"),
        fileInput("criteria", "Import criteria", multiple = FALSE, accept = ".csv"),
        actionButton("import", "Import", style = "margin-top:25px; width:150px;")
      ),
      tags$hr(),
      fillRow(
        flex = c(2,2),
        fillCol(
          flex = c(5,1),
          uiOutput("solution"),
          actionButton("updatesolution", "Update solution")
        ),
        fillCol(
          flex = c(5,1),
          rhandsontable::rHandsontableOutput("criteria"),
          actionButton("updatecriteria", "Update criteria")
        )
      ),
      tags$hr(),
      fillRow(
        flex = c(2,2,1,1,1,1),
        uiOutput("slctquest"),
        actionButton("backstart", "First student", style = "margin-top:25px; width:150px;"),
        actionButton("prevstud", "Previous student", style = "margin-top:25px; width:150px;"),
        textOutput("slctstudent"),
        actionButton("nextstud", "Next student", style = "margin-top:25px; width:150px;"),
        tags$br()
      ),
      tags$hr(),
      fillRow(
        flex = c(2,2),
        textOutput("answer"),
        fillCol(
          flex = c(5,1),
          rhandsontable::rHandsontableOutput("percents"),
          actionButton("updatepercents", "Update percents")
        )
      ),
      fillRow(
        #flex = c(1,11),
        #uiOutput("stats")
      )
    )
  )
  
  
  server <- function(input, output, session) {
    
    # Bind variables
    
    
    ################
    # Filter list and selection table creation
    
    tables <- reactiveValues()
    tables$studentincr <- 1
    
    #############
    # Import data
    
    observeEvent(input$import, {
      
      
      if (is.null(input$answers)) {
        if (file.exists("answers/answers.csv")){
          tables$answers <- as.data.frame(
            utils::read.csv(
              file = "answers/answers.csv",
              stringsAsFactors = FALSE
            ),
            stringsAsFactors = FALSE
          )
        } else {
          tables$answers <- data.frame(
            studentid = as.character(NA),
            questionid = as.character(NA),
            answer = as.character(NA)
          )
        }
      } else {
        tables$answers <- as.data.frame(
          utils::read.csv(
            file = input$answers$datapath[[1]],
            stringsAsFactors = FALSE
          ),
          stringsAsFactors = FALSE
        )
      }
      
      stopifnot(nrow(na.omit(tables$answers))>0)
      
      if (is.null(input$solutions)) {
        if (file.exists("parameters/solutions.csv")){
          tables$solutions <- as.data.frame(
            utils::read.csv(
              file = "parameters/solutions.csv",
              stringsAsFactors = FALSE
            ),
            stringsAsFactors = FALSE
          )
        } else {
          tables$solutions <- data.frame(
            questionid = unique(tables$answers$questionid),
            solution = as.character(NA)
          )
        }
      } else {
        tables$solutions <- as.data.frame(
          utils::read.csv(
            file = input$solutions$datapath[[1]],
            stringsAsFactors = FALSE
          ),
          stringsAsFactors = FALSE
        )
      }
      
      missingquest <- setdiff(unique(tables$answers$questionid), unique(tables$solutions$questionid))
      if (length(missingquest)>0) for (i in missingquest) {
        add <- data.frame(
          questionid = i,
          solution = as.character(NA)
        )
        tables$solutions <- dplyr::bind_rows(tables$solutions, add)
      }
      
      
      if (is.null(input$criteria)) {
        if (file.exists("parameters/criteria.csv")){
          tables$criteria <- as.data.frame(
            utils::read.csv(
              file = "parameters/criteria.csv",
              stringsAsFactors = FALSE
            ),
            stringsAsFactors = FALSE
          )
        } else {
          tables$criteria <- data.frame(
            questionid = unique(tables$answers$questionid),
            criterion = as.character(NA),
            weight = as.double(NA)
          )
        }
      } else {
        tables$criteria <- as.data.frame(
          utils::read.csv(
            file = input$criteria$datapath[[1]],
            stringsAsFactors = FALSE
          ),
          stringsAsFactors = FALSE
        )
      }
      
      missingquest <- setdiff(unique(tables$answers$questionid), unique(tables$criteria$questionid))
      if (length(missingquest)>0) for (i in missingquest) {
        add <- data.frame(
          questionid = i,
          criterion = as.character(NA),
          order = 1,
          weight = 1
        )
        tables$solutions <- dplyr::bind_rows(tables$solutions, add)
      }
      
      
      tables$questions <- sort(unique(tables$answers$questionid))
      students <- students <- unique(tables$answers$studentid)
      tables$students <- students[sample(seq_len(length(students)), length(students), replace = FALSE)]
      
      tables$percents <- tibble::tibble(
        studentid = tables$students,
        questionid = list(tables$questions)
      ) %>%
        tidyr::unnest(questionid) %>%
        dplyr::left_join(tables$criteria, by = "questionid") %>%
        dplyr::select(-weight) %>%
        dplyr::mutate(percents = 0)
    })
    
    
    ###########
    # Create UI
    output$slctquest <- renderUI({
      selectInput("slctquest", "Select a question", choices = tables$questions, selected = tables$questions[[1]], width = "75%")
    })
    
    observeEvent(input$backstart, {
      tables$studentincr <- 1
    })
    
    observeEvent(input$prevstud, {
      tables$studentincr <- max(1,tables$studentincr - 1)
    })
    
    observeEvent(input$nextstud, {
      tables$studentincr <- min(tables$studentincr + 1, length(tables$students))
    })
    
    output$slctstudent <- renderText(
      paste0("Student number ", tables$studentincr, " out of ", length(tables$students), ".")
    )
    
    output$solution <- renderUI({
      if (!is.null(tables$solutions) & !is.null(tables$studentincr)){
        textsol <- tables$solutions %>%
          dplyr::filter(
            questionid == input$slctquest
          ) %>%
          dplyr::select(solution)
        textsol <- textsol$solution[1]
      } else textsol <- c("")
      textAreaInput("solution", label = "Solution", value = textsol, width = "800px", height = "200px")
    })
    
    output$answer <- renderText(
      if (!is.null(tables$answers) & !is.null(tables$studentincr)){
        tables$answers %>%
          dplyr::filter(
            studentid == tables$students[[tables$studentincr]],
            questionid == input$slctquest
          ) %>%
          dplyr::select(answer) %>%
          unlist() %>% as.character()
      }
      
    )
    
    output$criteria <- rhandsontable::renderRHandsontable(
      if (!is.null(tables$solutions) & !is.null(tables$studentincr)){
        tables$criteria %>%
          dplyr::filter(
            questionid == input$slctquest
          ) %>%
          rhandsontable::rhandsontable(height = 250, width = "100%", rowHeaders = NULL) %>%
          rhandsontable::hot_col("questionid", width = 1) %>%
          rhandsontable::hot_col("criterion", width = 550) %>%
          rhandsontable::hot_col("order", width = 50) %>%
          rhandsontable::hot_col("weight", width = 100, format = "0%") %>%
          rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
      }
    )
    
    output$percents <- rhandsontable::renderRHandsontable(
      if (!is.null(tables$percents) & !is.null(tables$studentincr)){
        tables$percents %>%
          dplyr::filter(
            studentid == tables$students[[tables$studentincr]],
            questionid == input$slctquest
          ) %>%
          rhandsontable::rhandsontable(height = 250, width = "100%", rowHeaders = NULL) %>%
          rhandsontable::hot_col("studentid", width = 1) %>%
          rhandsontable::hot_col("questionid", width = 1) %>%
          rhandsontable::hot_col("criterion", width = 600) %>%
          rhandsontable::hot_col("order", width = 1) %>%
          rhandsontable::hot_col("percents", width = 100, format = "0%") %>%
          rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      }
    )
    
    #############
    # Update data
    
    
    
    
    #########
    # On exit
    
    observeEvent(input$done, {
      
      stopApp()
    })
  }
  
  runGadget(ui, server, viewer = shiny::browserViewer())
}
