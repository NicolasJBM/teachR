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
#' @importFrom readxl read_xlsx
#' @importFrom WriteXLS WriteXLS
#' @export


calibRate <- function() {
  ui <- miniPage(
    theme = shinythemes::shinytheme("flatly"),
    tags$head(tags$style(
      HTML(".shiny-notification {
              position:fixed;top: 30%;left: 0%;right: 0%;
           }")
    )),

    gadgetTitleBar("Item characteristics and weights"),
    miniTabstripPanel(
      miniTabPanel(
        "Import",
        icon = icon("sliders"),
        miniContentPanel(
          fillRow(
            flex = c(1,1,1,1),
            fileInput(
              "getsolutions",
              "Select solution files",
              multiple = TRUE,
              accept = ".xlsx"
            ),
            fileInput(
              "getcriteria",
              "Select criteria files",
              multiple = TRUE,
              accept = ".xlsx"
            ),
            fileInput(
              "getgrades",
              "Select grades files",
              multiple = TRUE,
              accept = ".xlsx"
            ),
            actionButton("import", "Import files")
          )
        )
      ),

      miniTabPanel(
        "Weights",
        icon = icon("balance-scale"),
        miniContentPanel(
          fillCol(
            flex = c(1,1),
            fillRow(
              flex = c(5,1),
              rHandsontableOutput("dispcriteria"),
              actionButton("updateweights", "Update")
            ),
            plotOutput("distribution")
          )
        )
      ),
      
      miniTabPanel(
        "Analyses",
        icon = icon("balance-scale"),
        miniContentPanel(
          
        )
      )
    )
  )


  server <- function(input, output, session) {

    # Bind variables


    ################
    tables <- reactiveValues()
    
    observeEvent(input$import, {
      
      solutions <- list()
      for (i in seq_len(nrow(input$getsolutions))){
        solutions[[i]] <- readxl::read_excel(input$getsolutions$datapath[[i]])
      }
      solutions <- dplyr::bind_rows(solutions)
      tables$solutions <- solutions
      
      grades <- list()
      for (i in seq_len(nrow(input$getgrades))){
        grades[[i]] <- readxl::read_excel(input$getgrades$datapath[[i]])
      }
      grades <- dplyr::bind_rows(grades)
      tables$grades <- grades
      
      criteria <- list()
      for (i in seq_len(nrow(input$getcriteria))){
        criteria[[i]] <- readxl::read_excel(input$getcriteria$datapath[[i]])
      }
      criteria <- dplyr::bind_rows(criteria) %>%
        dplyr::group_by(question_id) %>%
        tidyr::nest() %>%
        dplyr::left_join(
          dplyr::select(
            solutions, question_id, points
          ),
          by = "question_id"
        ) %>%
        dplyr::mutate(
          weight = points,
          divide = purrr::map_int(data, nrow)
        ) %>%
        dplyr::mutate(points = points / divide) %>%
        dplyr::mutate(weight = weight / divide) %>%
        dplyr::select(question_id, data, weight, points) %>%
        tidyr::unnest(data) %>%
        dplyr::ungroup()
      tables$criteria <- criteria
      
    })
    
    output$dispcriteria <- renderRHandsontable({
      tables$criteria %>%
        rhandsontable::rhandsontable(
          height = 400,
          width = "100%",
          stretchH = "all"
        ) %>%
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE,
          allowColEdit = FALSE
        )
    })
    
    observeEvent(input$updateweights, {
      tables$criteria <- rhandsontable::hot_to_r(input$dispcriteria)
    })
    
    scores <- reactive({
      tables$grades %>%
        dplyr::left_join(
          dplyr::select(
            tables$criteria,
            criterion_id, weight, points
          ),
          by = "criterion_id"
        ) %>%
        dplyr::mutate(score = round(grade * weight, 2)) %>%
        dplyr::group_by(student_id, question_id) %>%
        dplyr::summarise(
          score = sum(score, na.rm = TRUE),
          total = sum(points, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          score = dplyr::case_when(
            score <= total ~ score,
            TRUE ~ total
          )
        )
    })
    
    output$distribution <- renderPlot({
      
      distrib <- scores() %>%
        dplyr::group_by(student_id) %>%
        dplyr::summarise(
          score = sum(score, na.rm = TRUE),
          total = sum(total, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          score = dplyr::case_when(
            score <= total ~ score,
            TRUE ~ total
          )
        ) %>%
        dplyr::mutate(score = round(score,0)) %>%
        dplyr::group_by(score) %>%
        dplyr::count() %>%
        dplyr::filter(score > 0)
      
      distrib %>%
        ggplot2::ggplot(
        ggplot2::aes(x = score, y = n)
      ) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::xlab("Credits") +
        ggplot2::ylab("Count")
    })



    #################
    # On exit

    observeEvent(input$done, {
      WriteXLS::WriteXLS(tables$criteria, "weights.xlsx")
      WriteXLS::WriteXLS(scores(), "scores.xlsx")
      stopApp()
    })
  }

  runGadget(ui, server, viewer = shiny::browserViewer())
}
