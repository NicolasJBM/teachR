#' Shiny gadget to create and send individual reports to students.
#' Launch the gadget, upload the results exported from the gadget gradexam,
#' look at students' profiles, and send individual reports to the mailing list.
#' @seealso genexam()
#' @seealso gradexam()
#' @seealso checkexam()
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
#' @importFrom shiny textOutput
#' @importFrom shiny uiOutput
#' @importFrom shiny plotOutput
#' @importFrom shiny actionButton
#' @importFrom shiny renderDataTable
#' @importFrom shiny renderUI
#' @importFrom shiny renderPlot
#' @importFrom shiny reactive
#' @importFrom shiny observeEvent
#' @importFrom shiny withProgress
#' @importFrom shiny incProgress
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom dplyr left_join
#' @importFrom dplyr everything
#' @importFrom dplyr desc
#' @importFrom tidyr gather
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom gmailr mime
#' @importFrom gmailr subject
#' @importFrom gmailr from
#' @importFrom gmailr to
#' @importFrom gmailr attach_file
#' @importFrom gmailr text_body
#' @importFrom gmailr send_message
#' @importFrom gmailr attach_part
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom readxl read_excel
#' @importFrom rmarkdown render
#' @importFrom stats reorder
#' @export


sendexam <- function() {
  ui <- miniPage(
    gadgetTitleBar("Create and Send Individual Reports"),
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
              multiple = FALSE),
            tags$hr(),
            dataTableOutput(
              outputId = "results"
              )
            )
          )
        ),
      miniTabPanel("Look",
        icon = icon("eye"),
        miniContentPanel(
          fillCol(
            flex = c(1, 2, 3),
            uiOutput(
              outputId = "select_rep"
              ),
            plotOutput(
              outputId = "density",
              height = "90%"
              ),
            plotOutput(
              outputId = "plotreports",
              height = "90%"
              )
            )
          )
        ),
      miniTabPanel("Send",
        icon = icon("paper-plane"),
        miniContentPanel(
          textInput(
            inputId = "from",
            label = "Address of the sender"
            ),
          textInput(
            inputId = "subject",
            label = "Subject",
            value = "Individual exam report"
            ),
          textAreaInput(
            inputId = "message",
            label = "Message",
            value = "\nyou will find enclosed in this e-mail your individual answer sheet and report for the exam. Please notify me if you notice an error in the evaluation.\n\nBest regard.",
            width = "700px",
            height = "200px"
            ),
          actionButton(
            inputId = "send",
            label = "Send all reports"
            )
        )
      )
    )
  )
  
  
  server <- function(input, output, session) {
    
    # Visibly bind variables (avoid notes in checks)
    exam_id <- NULL
    student_id <- NULL
    question <- NULL
    solution <- NULL
    answer <- NULL
    score <- NULL
    level <- NULL
    topic <- NULL
    you <- NULL
    scope <- NULL
    points <- NULL
    firstname <- NULL
    lastname <- NULL
    email <- NULL
    percentage <- NULL

    # First page: upload and display the results of the exam
    results <- reactive({
      if (!is.null(input$results)) {
        readxl::read_excel(path = input$results$datapath[[1]])
      }
    })
    
    output$results <- renderDataTable(results(), options = list(pageLength = 10))


    # Second page: prepare reports and check students profile
    output$select_rep <- renderUI({
      if (!is.null(results())) {
        list(
          fillRow(
            flex = c(1, 1),
            selectInput(inputId = "slct_student",
                        label = "Student id",
                        choices = na.omit(unique(results()$student_id))),
            selectInput(inputId = "slct_level",
                        label = "Select the level",
                        choices = c("chapter", "section", "subsection", "objective"),
                        selected = "chapter")
          )
        )
      }
    })

    answers <- reactive({
      if (!is.null(results())) {
        dplyr::select(
          results(),
          student_id, exam_id, question,
          points, solution, answer, score
          )
      }
    })

    reports <- reactive({
      if (!is.null(results())) {
        report <- results() %>%
          dplyr::group_by(student_id) %>%
          tidyr::nest() %>%
          dplyr::mutate(
            chapter = map(data,
                          synthesize,
                          level = "chapter"),
            section = map(data,
                          synthesize,
                          level = "section"),
            subsection = map(data,
                             synthesize,
                             level = "subsection"),
            objective = map(data,
                            synthesize,
                            level = "objective")
          ) %>%
          dplyr::select(-data) %>%
          tidyr::gather(level,
                        data,
                        -student_id) %>%
          tidyr::unnest() %>%
          dplyr::ungroup() %>%
          dplyr::select(student_id, level,
                        topic, you = percentage)

        avg <- report %>%
          dplyr::select(-student_id) %>%
          dplyr::group_by(level, topic) %>%
          dplyr::summarize(you = mean(you, na.rm = TRUE)) %>%
          dplyr::select(level, topic, group = you)

        report %>%
          dplyr::left_join(avg, by = c("level", "topic")) %>%
          tidyr::gather(scope, percentage,
                        -student_id, -level, -topic)
      }
    })

    # Add plot with grade distribution and position of the student.
    output$density <- renderPlot({
      if (!is.null(results()) & !is.null(input$slct_student)) {
        base <- results() %>%
          dplyr::select(student_id, score) %>%
          dplyr::group_by(student_id) %>%
          dplyr::summarize(score = sum(score, na.rm = TRUE)) %>%
          dplyr::ungroup()

        student_score <- filter(base, student_id == input$slct_student)$score
        
        group_mean <- base %>%
          dplyr::select(-student_id) %>%
          dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
          as.numeric()

        if (student_score >= group_mean) col <- "darkgreen" else col <- "red"

        base %>%
          ggplot(aes(x = score)) +
          geom_density() +
          geom_vline(xintercept = student_score, lty = 2, color = col) +
          annotate("text", x = student_score, y = 0.05, label = "Your\ngrade", color = col) +
          geom_vline(xintercept = group_mean, lty = 2, color = "blue") +
          annotate("text", x = group_mean, y = 0.025, label = "Group\nmean", color = "lightblue")
      }
    })


    output$plotreports <- renderPlot({
      if (!is.null(reports()) &
          !is.null(input$slct_student) &
          !is.null(input$slct_level)) {
        
        base <- results() %>%
          dplyr::select(student_id, score) %>%
          dplyr::group_by(student_id) %>%
          dplyr::summarize(score = sum(score, na.rm = TRUE)) %>%
          dplyr::ungroup()
        
        student_score <- filter(base, student_id == input$slct_student)$score
        
        group_mean <- base %>%
          dplyr::select(-student_id) %>%
          dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
          as.numeric()
        
        if (student_score >= group_mean) col <- "darkgreen" else col <- "red"
        
        reports() %>%
          dplyr::filter(student_id == input$slct_student, level == input$slct_level) %>%
          dplyr::mutate(topic = reorder(topic, dplyr::desc(topic))) %>%
          ggplot(aes(x = topic, y = percentage, fill = scope)) +
          geom_bar(stat = "identity", width = 0.5, position = position_dodge(width = 0.5)) +
          scale_fill_manual(values = c("lightblue", col)) +
          coord_flip() +
          ylim(0, 100)
      }
    })


    # Third page
    observeEvent(input$send, {
      results <- results()
      answers <- answers()
      reports <- reports()

      # Check that the reports directory exists in the working directory
      ifelse(!dir.exists(file.path(getwd(), "reports")), dir.create(file.path(getwd(), "reports")), FALSE)

      # Save data and parameters in reports directory
      save(results, answers, reports, file = "reports/exam_data.RData")

      # Create student list
      student_list <- results %>%
        dplyr::select(student_id, firstname, lastname, email, exam_id) %>%
        unique()

      # Loop through student list to produce and send the report

      withProgress(message = "Sending reports", value = 0, {
        steps <- length(student_list$student_id)
        wd <- getwd()
        setwd(paste0(getwd(), "/reports"))

        for (i in 1:steps) {
          incProgress(i / steps, detail = "Generating...")

          student_id <- student_list$student_id[[i]]
          first_name <- student_list$firstname[[i]]
          last_name <- student_list$lastname[[i]]
          e_mail <- student_list$email[[i]]

          save(student_id, first_name, last_name, file = "student_info.RData")

          bodytxt <- as.character(paste0(
            paste0("Dear ", first_name, " ", last_name, ",\n"),
            input$message
          ))

          rmarkdown::render(paste0(getwd(), "/ind_rep.Rmd"),
            output_dir = getwd(),
            output_file = paste0(student_id, ".pdf")
          )

          message <- gmailr::mime() %>%
            gmailr::from(input$from) %>%
            gmailr::to(e_mail) %>%
            gmailr::subject(input$subject) %>%
            gmailr::text_body(bodytxt) %>%
            gmailr::attach_part(bodytxt) %>%
            gmailr::attach_file(filename = paste0(student_id, ".pdf"))
          
          send_message(message)

          Sys.sleep(2)
        }

        setwd(wd)
      })

      stopApp()
    })

    #################
    # On exit

    observeEvent(input$done, {
      stopApp()
    })
  }
  runGadget(ui, server)
}


synthesize <- function(x, level) {
  score <- NULL
  topic <- NULL
  points <- NULL

  x <- x[, c(level, "score", "points")]
  names(x) <- c("topic", "score", "points")
  x %>%
    dplyr::group_by(topic) %>%
    dplyr::summarize(
      score = mean(score, na.rm = TRUE),
      points = mean(points, na.rm = TRUE)
    ) %>%
    dplyr::mutate(percentage = round(100 * score / points, 0)) %>%
    dplyr::select(-score, -points) %>%
    dplyr::ungroup()
}
