#' Shiny gadget guiding through the creation of a question: selection of a template, assignment to a topic, and entry in relevant databases.
#' @return Create a question based on the requested template and make entries in the structure databases.
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


change_structure <- function() {
  
  ui <- miniPage(
    theme = shinythemes::shinytheme("flatly"),
    
    gadgetTitleBar("Create a question"),
    miniTabstripPanel(
      miniTabPanel(
        "Create",
        icon = icon("sliders"),
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
    viewer = shiny::paneViewer(
      width = 1800,
      height = 1200
    )
  )
}
