#' Shiny gadget to change and update the structure of a package.
#' @return Make necessary entries in structure databases.
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


calibRate <- function() {
  ui <- miniPage(
    theme = shinythemes::shinytheme("flatly"),
    tags$head(tags$style(
      HTML(".shiny-notification {
              position:fixed;top: 30%;left: 0%;right: 0%;
           }")
    )),
    
    gadgetTitleBar("Grade open questions and essays"),
    miniTabstripPanel(
      miniTabPanel("Import",
                   icon = icon("sliders"),
                   miniContentPanel(
                     
                   )
      ),
      
      
      miniTabPanel("Items",
                   icon = icon("filter"),
                   miniContentPanel(
                     
                   )
      ),
      
      miniTabPanel("Students",
                   icon = icon("list-ol"),
                   miniContentPanel(
                     
                   )
      ),
      
      miniTabPanel("Export",
                   icon = icon("list-ol"),
                   miniContentPanel(
                     
                   )
      ),
      
      
      miniTabPanel("Check",
                   icon = icon("eye"),
                   miniContentPanel(
                     
                   )
      ),
      
      
      miniTabPanel("Balance",
                   icon = icon("balance-scale"),
                   miniContentPanel(
                     
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
          SU = "tmp",
          L1 = "tmp",
          L2 = "tmp",
          L3 = "tmp",
          L4 = "tmp",
          SD = 0,
          PT = 0,
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
    })

    
    #################
    # On exit
    
    observeEvent(input$done, {
      
      stopApp()
    })
  }
  
  runGadget(
    ui,
    server,
    viewer = dialogViewer(
      dialogName = "genexam", width = 1800, height = 1200)
    )
}
