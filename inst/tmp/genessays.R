#' Gadget to facilitate grading essays
#' @return What the coder decides to return.
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
#' @importFrom colourpicker colourInput
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
#' @importFrom stringr str_replace_all
#' @importFrom utils data
#' @importFrom utils read.csv
#' @importFrom writR stat_totals
#' @export


genessays <- function() {
  
  ui <- miniPage(
    theme = shinytheme("spacelab"),
    
    gadgetTitleBar("Grade Essays"),
    miniTabstripPanel(
      miniTabPanel("Import Essays",
        icon = icon("upload"),
        miniContentPanel(
          fillCol(
            flex = c(1,1,1,3),
            fillRow(
              flex = c(1,1),
              fileInput(
                "studentlist",
                "Select the student list:",
                multiple = TRUE,
                accept = c(".xlsx")
              ),
              fileInput(
                "documents",
                "Select the documents:",
                multiple = TRUE,
                accept = c(".txt",".rtf",".doc",".docx")
              )
            ),
            fillRow(
              flex = c(1,1),
              fileInput(
                "rubric",
                "Select the rubric:",
                multiple = FALSE,
                accept = c(".xlsx")
              ),
              fileInput(
                "typcomment",
                "Select the typical comments:",
                multiple = FALSE,
                accept = c(".xlsx")
              )
            ),
            fillRow(
              flex = c(1,1),
              fileInput(
                "answerkey",
                "Select the answer key:",
                multiple = FALSE,
                accept = c(".xlsx")
              ),
              fileInput(
                "keywords",
                "Select the keywords:",
                multiple = FALSE,
                accept = c(".xlsx")
              )
            ),
            actionButton("create","Create project")
          )
        )
      ),
      miniTabPanel("Assign sections",
                   icon = icon("eye"),
                   miniContentPanel(
                     
                   )
      )
    )
  )
  

  server <- function(input, output, session) {

    # Bind variables
    some_variable <- NULL
    
    # Create a list where reactive values can be stored to be later updated through observers
    values <- reactiveValues()
    values$data <- NA
    
    
    # List of action to do when exiting
    observeEvent(input$done, {
      
      
      stopApp()
    })
  }
  runGadget(ui, server, viewer = paneViewer(minHeight = "maximize"))
}
