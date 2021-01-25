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
        icon = icon("calculator"),
        miniContentPanel(
          fillCol(
            flex = c(2,6),
            fillRow(
              flex = c(1,2,3,1,1),
              uiOutput("slctpart"),
              uiOutput("slctquest"),
              uiOutput("slctcrit"),
              selectInput(
                "slctana",
                "Analysis:",
                choices = c("PCA","IRT"),
                selected = "PCA",
                multiple = FALSE,
                width = "100%"
              ),
              actionButton("run", "Run analysis")
            ),
            fillRow(
              flex = c(1,3),
              dataTableOutput("table"),
              plotOutput("plot", width = "90%", height = "600px")
            )
          )
        )
      )
    )
  )


  server <- function(input, output, session) {

    # Bind variables
    criterion_id <- NULL
    data <- NULL
    divide <- NULL
    grade <- NULL
    points <- NULL
    question_id <- NULL
    score <- NULL
    student_id <- NULL
    total <- NULL
    weight <- NULL
    MR1 <- NULL
    PC1 <- NULL
    difficulty <- NULL
    discrimination <- NULL
    loadings <- NULL
    part <- NULL


    ############################################################################
    # Import
    
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
      tables$analysis <- NA
    })
    
    
    ############################################################################
    # Adjust
    
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
        dplyr::mutate(score = round(score*4,0)/4) %>%
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


    ############################################################################
    # Analyses
    
    output$slctpart <- renderUI({
      selectInput(
        "slctpart",
        "Parts:",
        choices = unique(tables$grades$part),
        selected = unique(tables$grades$part),
        multiple = TRUE,
        width = "100%"
      )
    })
    
    output$slctquest <- renderUI({
      if (!is.null(input$slctpart)){
        base <- dplyr::filter(tables$grades, part %in% input$slctpart)
        selectInput(
          "slctquest",
          "Questions:",
          choices = unique(base$question_id),
          selected = unique(base$question_id),
          multiple = TRUE,
          width = "100%"
        )
      }
    })

    output$slctcrit <- renderUI({
      if (!is.null(input$slctquest)){
        base <- dplyr::filter(tables$grades, question_id %in% input$slctquest)
        selectInput(
          "slctcrit",
          "Criteria:",
          choices = unique(base$criterion_id),
          selected = unique(base$criterion_id),
          multiple = TRUE,
          width = "100%"
        )
      }
    })
    
    observeEvent(input$run, {
      
      base_analysis <- tables$grades %>%
        dplyr::filter(criterion_id %in% input$slctcrit) %>%
        dplyr::select(student_id, criterion_id, grade) %>%
        tidyr::pivot_wider(
          names_from = "criterion_id",
          values_from = "grade"
        ) %>%
        dplyr::select(-student_id)
      
      if (input$slctana == "PCA"){
        pc <- psych::principal(base_analysis)
        tables$analysis <- as.data.frame(pc$loadings[]) %>%
          tibble::rownames_to_column("criterion_id") %>%
          dplyr::select(criterion_id, loadings = PC1)
        
      } else {
        
        irt <- psych::irt.fa(
          dplyr::mutate_all(
            base_analysis,
            function(x) as.numeric(x > 0)
          )
        )
        
        tables$analysis <- irt$irt$discrimination %>%
          as.data.frame() %>%
          tibble::rownames_to_column("criterion_id") %>%
          dplyr::mutate(difficulty = irt$irt$difficulty[[1]]) %>%
          dplyr::select(criterion_id, difficulty, discrimination = MR1)
        
      }
    })
    
    output$table <- renderDataTable({
      if (length(tables$analysis) > 1) tables$analysis
    })
    
    output$plot <- renderPlot({
      if (length(tables$analysis) > 1){
        if (input$slctana == "PCA" & "loadings" %in% names(tables$analysis)){
          tables$analysis %>%
            ggplot2::ggplot(ggplot2::aes(x = criterion_id, y = loadings)) +
            ggplot2::geom_bar(stat = "identity") +
            ggplot2::coord_flip()
        } else {
          if ("difficulty" %in% names(tables$analysis)){
            tables$analysis %>%
              ggplot2::ggplot(ggplot2::aes(
                x = difficulty,
                y = discrimination,
                label = criterion_id
              )) +
              ggplot2::geom_label()
          }
        }
      }
    })
    
    ############################################################################
    # On exit

    observeEvent(input$done, {
      WriteXLS::WriteXLS(tables$criteria, "weights.xlsx")
      WriteXLS::WriteXLS(scores(), "scores.xlsx")
      stopApp()
    })
  }

  runGadget(ui, server, viewer = shiny::browserViewer())
}
