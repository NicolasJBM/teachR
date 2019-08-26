#' Shiny gadget to check and analyze results of an exam as exported from gradexam.
#' @return A list with all the last run analyses.
#' @seealso genexam()
#' @seealso gradexam()
#' @seealso sendexam()
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
#' @importFrom shiny observeEvent
#' @importFrom shiny withProgress
#' @importFrom shiny incProgress
#' @importFrom tibble column_to_rownames
#' @importFrom tibble tibble
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize_all
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr case_when
#' @importFrom dplyr everything
#' @importFrom tidyr spread
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_hline
#' @importFrom readxl read_excel
#' @importFrom WriteXLS WriteXLS
#' @importFrom psych irt.fa
#' @importFrom psych plot.irt
#' @importFrom graphics plot
#' @export


checkexam <- function() {
  ui <- miniPage(
    gadgetTitleBar("Check and Analyse Exam Results"),
    miniTabstripPanel(
      miniTabPanel("Upload",
        icon = icon("upload"),
        miniContentPanel(
          fillCol(
            flex = c(1, 1, 10),
            fileInput(
              inputId = "results",
              label = "Select the results file (.xlsx)",
              accept = c(".xlsx"),
              multiple = FALSE
            ),
            tags$hr(),
            dataTableOutput(outputId = "results")
          )
        )
      ),
      miniTabPanel("Check",
        icon = icon("check"),
        miniContentPanel(
          fillCol(
            flex = c(1, 1, 10),
            uiOutput(outputId = "slct_student"),
            tags$hr(),
            dataTableOutput(outputId = "check")
          )
        )
      ),
      miniTabPanel("Students",
        icon = icon("users"),
        miniContentPanel(
          fillCol(
            flex = c(1, 5),
            fillRow(
              flex = c(1, 1),
              checkboxInput(
                inputId = "sep_exam_student",
                label = "Compare exam versions",
                value = FALSE
              ),
              downloadButton(
                outputId = "dwldgrades",
                label = "Save grades"
              )
            ),
            plotOutput(outputId = "distribution")
          )
        )
      ),
      miniTabPanel("Questions",
        icon = icon("tasks"),
        miniContentPanel(
          fillCol(
            flex = c(1, 5),
            fillRow(
              flex = c(1, 1, 1),
              uiOutput(outputId = "slct_question"),
              checkboxInput(
                inputId = "sep_exam_question",
                label = "Compare exam versions",
                value = FALSE
              ),
              downloadButton(
                outputId = "dwldsuccessrates",
                label = "Save success rates"
              )
            ),
            plotOutput(outputId = "proportions")
          )
        )
      ),
      miniTabPanel("IRT",
        icon = icon("laptop"),
        miniContentPanel(
          fillCol(
            flex = c(1, 3),
            fillRow(
              flex = c(1, 1),
              fillCol(
                flex = c(1, 1),
                selectInput(
                  inputId = "slct_questgp",
                  label = "Select the group level",
                  choices = c("all", "chapter", "section", "subsection", "objective"),
                  selected = "all"
                ),
                checkboxInput(
                  inputId = "correct",
                  label = "Correct tetrachoric correlations",
                  value = TRUE
                )
              ),
              fillCol(
                flex = c(1, 1, 1),
                uiOutput(outputId = "slct_gp_irt"),
                selectInput(
                  inputId = "type_irt_plot",
                  label = "IRT plot type",
                  choices = c("ICC", "IIC", "test"),
                  selected = "ICC"
                ),
                downloadButton(
                  outputId = "dwldirt",
                  label = "Save IRT"
                )
              )
            ),
            plotOutput(outputId = "irt_plot")
          )
        )
      )
    )
  )




  server <- function(input, output, session) {


    # Visibly bind variables (avoid notes in checks)
    exam_id <- NULL
    student_id <- NULL
    solution <- NULL
    answer <- NULL
    correct <- NULL
    score <- NULL
    question <- NULL
    question_id <- NULL
    filt <- NULL

    # First page: upload and display exam results
    results <- reactive({
      if (!is.null(input$results)) {
        readxl::read_excel(path = input$results$datapath[[1]])
      }
    })

    output$results <- renderDataTable(results(), options = list(pageLength = 10))


    # Second page
    output$slct_student <- renderUI({
      if (!is.null(results())) {
        selectInput(
          inputId = "slct_student",
          label = "Select a student id",
          choices = c("Select", na.omit(unique(results()$student_id))),
          selected = "Select"
        )
      } else {
        renderText("Please select first a set of results.")
      }
    })

    check <- reactive({
      if (!is.null(input$slct_student)) {
        if (input$slct_student != "Select") {
          results() %>%
            dplyr::select(exam_id, student_id, solution, answer, correct, score) %>%
            dplyr::filter(student_id == input$slct_student)
        }
      }
    })

    output$check <- renderDataTable(check(), options = list(pageLength = 45))

    # Third page
    grades <- reactive({
      if (!is.null(results())) {
        results() %>%
          dplyr::select(student_id, exam_id, score) %>%
          dplyr::mutate(
            student_id = as.character(student_id),
            exam_id = as.character(exam_id)
          ) %>%
          dplyr::group_by(student_id, exam_id) %>%
          dplyr::summarize_all(sum, na.rm = TRUE) %>%
          dplyr::ungroup()
      } else {
        renderText("Please select first a set of results.")
      }
    })

    output$distribution <- renderPlot({
      if (!is.null(grades()) &
        !is.null(input$sep_exam_student)) {
        if (input$sep_exam_student == FALSE) {
          ggplot(grades(), aes(x = score)) +
            geom_density() +
            geom_vline(xintercept = mean(grades()$score), lty = 2)
        } else {
          ggplot(grades(), aes(x = score, group = exam_id, fill = exam_id, color = exam_id)) +
            geom_density(alpha = 0.2) +
            geom_vline(xintercept = mean(grades()$score), lty = 2)
        }
      }
    })

    output$dwldgrades <- downloadHandler(
      filename = function() {
        paste("grades.xlsx")
      },
      content = function(file) {
        WriteXLS(grades(), file)
      }
    )


    # Fourth page
    successrate <- reactive({
      if (!is.null(results()) & !is.null(input$sep_exam_question)) {
        if (input$sep_exam_question == FALSE) {
          results() %>%
            dplyr::select(question, question_id, correct) %>%
            dplyr::mutate(
              question = as.character(question),
              question_id = as.character(question_id)
            ) %>%
            dplyr::group_by(question, question_id) %>%
            dplyr::summarize_all(mean, na.rm = TRUE) %>%
            dplyr::ungroup()
        } else {
          results() %>%
            dplyr::select(question, question_id, exam_id, correct) %>%
            dplyr::mutate(
              question = as.character(question),
              question_id = as.character(question_id),
              exam_id = as.character(exam_id)
            ) %>%
            dplyr::group_by(question, question_id, exam_id) %>%
            dplyr::summarize_all(mean, na.rm = TRUE) %>%
            dplyr::ungroup()
        }
      } else {
        renderText("Please select first a set of results.")
      }
    })

    output$slct_question <- renderUI({
      if (!is.null(successrate())) {
        selectInput(
          inputId = "slct_question",
          label = "Select a question",
          choices = c("Select", na.omit(unique(successrate()$question_id))),
          selected = "Select"
        )
      } else {
        renderText("Please select first a set of results.")
      }
    })

    output$proportions <- renderPlot({
      if (!is.null(successrate()) &
        !is.null(input$slct_question)) {
        if (input$slct_question == "Select") {
          sr <- successrate()
        } else {
          sr <- successrate() %>%
            dplyr::filter(question_id == input$slct_question)
        }
        if (input$sep_exam_question == FALSE) {
          sr %>%
            ggplot(aes(x = question_id, y = correct)) +
            geom_bar(stat = "identity") +
            geom_hline(yintercept = 0.5, lty = 2) +
            ylim(0, 1) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        } else {
          sr %>%
            dplyr::group_by(question, question_id, exam_id) %>%
            dplyr::summarize_all(mean, na.rm = TRUE) %>%
            dplyr::ungroup() %>%
            ggplot(aes(x = question_id, y = correct, group = exam_id, fill = exam_id, color = exam_id)) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_hline(yintercept = 0.5, lty = 2) +
            ylim(0, 1) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        }
      }
    })

    output$dwldsuccessrates <- downloadHandler(
      filename = function() {
        paste("success_rates.xlsx")
      },
      content = function(file) {
        WriteXLS(successrate(), file)
      }
    )

    # Fifth page
    output$slct_gp_irt <- renderUI({
      if (!is.null(input$slct_questgp) & !is.null(results())) {
        if (input$slct_questgp != "all") {
          choices <- as.character(na.omit(unique(results()[[input$slct_questgp]])))
          selectInput(
            inputId = "questgp",
            label = "Display analysis for:",
            choices = choices,
            selected = choices[1]
          )
        }
      }
    })

    irt_base <- reactive({
      if (!is.null(results())) {
        if (!is.null(input$slct_questgp)) {
          if (input$slct_questgp != "all" &
            !is.null(input$questgp)) {
            selection <- results() %>%
              dplyr::select(filt = input$slct_questgp, question_id) %>%
              dplyr::filter(filt == input$questgp)
            selection <- unique(as.character(unlist(selection$question_id)))
          } else {
            selection <- unique(as.character(unlist(results()$question_id)))
          }
          results() %>%
            dplyr::select(student_id, question_id, correct) %>%
            dplyr::filter(question_id %in% selection) %>%
            tidyr::spread(question_id, correct) %>%
            na.omit() %>%
            as.data.frame() %>%
            tibble::column_to_rownames("student_id")
        }
      }
    })

    irt_results <- reactive({
      if (!is.null(irt_base()) &
        !is.null(input$correct)) {
        if (length(irt_base()) > 1) {
          irt_results <- psych::irt.fa(
            irt_base(),
            nfactors = 1,
            correct = input$correct,
            plot = FALSE
          )
        } else {
          irt_results <- NULL
        }
      } else {
        irt_results <- NULL
      }
      irt_results
    })

    output$irt_plot <- renderPlot({
      if (!is.null(irt_results()) &
        !is.null(input$type_irt_plot)) {
        plot(irt_results(), type = input$type_irt_plot)
      }
    })

    irt_synthesis <- reactive({
      if (!is.null(irt_base()) & !is.null(irt_results())) {
        difficulty <- data.frame(
          question_id = row.names(as.data.frame(irt_results()$irt$difficulty)),
          difficulty = as.data.frame(irt_results()$irt$difficulty)[, 1]
        )
        discrimination <- data.frame(
          question_id = row.names(irt_results()$irt$discrimination),
          discrimination = irt_results()$irt$discrimination[, 1]
        )
        success <- data.frame(
          question_id = row.names(as.data.frame(colMeans(irt_base()))),
          success_rate = as.data.frame(colMeans(irt_base()))[, 1]
        )

        success %>%
          dplyr::left_join(difficulty, by = "question_id") %>%
          dplyr::left_join(discrimination, by = "question_id")
      }
    })

    output$dwldirt <- downloadHandler(
      filename = function() {
        paste("irt_synthesis.xlsx")
      },
      content = function(file) {
        WriteXLS(irt_synthesis(), file)
      }
    )

    #################
    # On exit

    observeEvent(input$done, {
      analyses <- list()
      if (!is.null(grades())) analyses[["grades"]] <- grades()
      if (!is.null(successrate())) analyses[["success_rates"]] <- successrate()
      if (!is.null(irt_results())) analyses[["irt_base"]] <- irt_base()
      if (!is.null(irt_results())) analyses[["irt_results"]] <- irt_results()
      if (!is.null(irt_synthesis())) analyses[["irt_synthesis"]] <- irt_synthesis()

      stopApp(analyses)
    })
  }
  runGadget(ui, server)
}
