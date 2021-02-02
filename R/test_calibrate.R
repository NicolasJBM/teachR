#' @name test_calibrate
#' @title Analyze and weight criteria
#' @author Nicolas Mangin
#' @description Analyze item characteirtics to guide the weighting of criteria
#' @return Export updated weights and scores
#' @import miniUI
#' @import shiny
#' @importFrom bslib bs_theme
#' @importFrom bslib font_google
#' @importFrom readxl read_xlsx
#' @importFrom WriteXLS WriteXLS
#' @export


test_calibrate <- function() {
  ui <- miniPage(
    theme = bslib::bs_theme(
      bootswatch = "flatly",
      base_font = bslib::font_google("Open Sans"),
      "enable-gradients" = TRUE,
      "enable-shadows" = TRUE,
      spacer = "0.5rem"
    ),

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
            flex = c(1, 1, 1, 1, 1),
            checkboxInput("weighted", "Weighted criteria", value = FALSE),
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
            fileInput(
              "getsolutions",
              "Select solution files",
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
            flex = c(5, 1, 4),
            rHandsontableOutput("dispcriteria"),
            fillRow(
              flex = c(1, 1, 1, 1, 1, 1),
              actionButton("updateweights", "Update"),
              numericInput(
                "threshold",
                "Passing grade",
                min = 0,
                value = 50,
                max = 100,
                step = 0.5
              ),
              textOutput("maxweight"),
              textOutput("maxpoints"),
              textOutput("studnbr"),
              textOutput("passrate")
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
            flex = c(2, 6),
            fillRow(
              flex = c(1, 2, 3, 1, 1),
              uiOutput("slctpart"),
              uiOutput("slctquest"),
              uiOutput("slctcrit"),
              selectInput(
                "slctana",
                "Analysis:",
                choices = c("PCA", "IRT"),
                selected = "PCA",
                multiple = FALSE,
                width = "100%"
              ),
              actionButton("run", "Run analysis")
            ),
            fillRow(
              flex = c(1, 3),
              dataTableOutput("table"),
              fillCol(
                flex = c(3, 1),
                plotOutput("plot", width = "90%", height = "600px"),
                plotOutput("subdistribution", width = "90%", height = "200px")
              )
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
    pass <- NULL


    ############################################################################
    # Import

    tables <- reactiveValues()

    observeEvent(input$import, {
      solutions <- list()
      for (i in seq_len(nrow(input$getsolutions))) {
        solutions[[i]] <- readxl::read_excel(input$getsolutions$datapath[[i]])
      }
      solutions <- dplyr::bind_rows(solutions)
      tables$solutions <- solutions

      grades <- list()
      for (i in seq_len(nrow(input$getgrades))) {
        grades[[i]] <- readxl::read_excel(input$getgrades$datapath[[i]])
      }
      grades <- dplyr::bind_rows(grades)

      grades <- grades %>%
        dplyr::group_by(question_id, part) %>%
        tidyr::nest() %>%
        dplyr::mutate(
          data = purrr::map(data, add_missing)
        ) %>%
        tidyr::unnest(data) %>%
        dplyr::ungroup()

      tables$grades <- grades

      criteria <- list()
      for (i in seq_len(nrow(input$getcriteria))) {
        criteria[[i]] <- readxl::read_excel(input$getcriteria$datapath[[i]])
      }

      if (input$weighted == FALSE) {
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
      } else {
        criteria <- dplyr::bind_rows(criteria)
      }

      tables$criteria <- criteria
      tables$analysis <- NA
      tables$subdistribution <- NA
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
          total = sum(points, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          score = round(score * 4, 0) / 4,
          total = round(total * 4, 0) / 4
        ) %>%
        dplyr::mutate(
          score = dplyr::case_when(
            score < 0 ~ 0,
            score <= total ~ score,
            total == 0 ~ score,
            TRUE ~ total
          )
        )
    })

    credits <- reactive({
      if (!is.null(scores()) & !is.null(input$threshold)) {
        scores() %>%
          dplyr::group_by(student_id) %>%
          dplyr::summarise(
            credits = sum(score, na.rm = TRUE),
            total = sum(total, na.rm = TRUE)
          ) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            credits = dplyr::case_when(
              credits < 0 ~ 0,
              credits <= total ~ credits,
              total == 0 ~ credits,
              TRUE ~ total
            )
          ) %>%
          dplyr::filter(credits > 0)
      }
    })

    output$maxweight <- renderText({
      maxweight <- round(sum(tables$criteria$weight), 2)
      paste0("Total weights: ", maxweight)
    })

    output$maxpoints <- renderText({
      maxpoints <- round(sum(tables$criteria$points), 0)
      paste0("Total points: ", maxpoints)
    })

    output$studnbr <- renderText({
      total <- nrow(credits())
      paste0("Number of students: ", total)
    })

    output$passrate <- renderText({
      if (!is.null(credits()) & !is.null(input$threshold)) {
        nbr <- sum(
          as.numeric(
            credits()$credits > input$threshold
          ),
          na.rm = TRUE
        )
        total <- nrow(credits())
        pct <- round(100 * nbr / total, 0)
        paste0("Success rate: ", pct, "%")
      } else {
        ""
      }
    })

    output$distribution <- renderPlot({
      if (!is.null(credits()) & !is.null(input$threshold)) {
        credits() %>%
          dplyr::mutate(credits = round(credits, 0)) %>%
          dplyr::group_by(credits) %>%
          dplyr::count() %>%
          dplyr::mutate(pass = credits >= input$threshold) %>%
          ggplot2::ggplot(
            ggplot2::aes(x = credits, y = n, fill = pass)
          ) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::xlab("Credits") +
          ggplot2::ylab("Count") +
          ggplot2::scale_fill_manual(values = c("red", "green"))
      }
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
      if (!is.null(input$slctpart)) {
        parts <- dplyr::filter(tables$grades, part %in% input$slctpart)
        selectInput(
          "slctquest",
          "Questions:",
          choices = unique(parts$question_id),
          selected = unique(parts$question_id),
          multiple = TRUE,
          width = "100%"
        )
      }
    })

    output$slctcrit <- renderUI({
      if (!is.null(input$slctquest)) {
        criteria <- dplyr::filter(
          tables$grades,
          question_id %in% input$slctquest
        )
        selectInput(
          "slctcrit",
          "Criteria:",
          choices = unique(criteria$criterion_id),
          selected = unique(criteria$criterion_id),
          multiple = TRUE,
          width = "100%"
        )
      }
    })









    observeEvent(input$run, {
      base_analysis <- tables$grades %>%
        dplyr::filter(criterion_id %in% input$slctcrit) %>%
        dplyr::select(student_id, criterion_id, grade)

      tables$subdistribution <- base_analysis %>%
        dplyr::left_join(
          dplyr::select(
            tables$criteria,
            criterion_id, weight, points
          ),
          by = "criterion_id"
        ) %>%
        dplyr::mutate(score = round(grade * weight, 2)) %>%
        dplyr::group_by(student_id) %>%
        dplyr::summarise(
          credits = round(sum(score, na.rm = TRUE), 0),
          total = round(sum(total, na.rm = TRUE), 0)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          credits = dplyr::case_when(
            credits < 0 ~ 0,
            credits <= total ~ credits,
            total == 0 ~ credits,
            TRUE ~ total
          )
        ) %>%
        dplyr::filter(credits > 0) %>%
        dplyr::group_by(credits) %>%
        dplyr::count()

      base_analysis <- base_analysis %>%
        tidyr::pivot_wider(
          names_from = "criterion_id",
          values_from = "grade",
          values_fill = 0
        ) %>%
        dplyr::select(-student_id) %>%
        dplyr::select_if(function(x) stats::sd(x) != 0)

      if (input$slctana == "PCA") {
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
      if (length(tables$analysis) > 1) {
        if (input$slctana == "PCA" & "loadings" %in% names(tables$analysis)) {
          tables$analysis %>%
            ggplot2::ggplot(ggplot2::aes(x = criterion_id, y = loadings)) +
            ggplot2::geom_bar(stat = "identity") +
            ggplot2::coord_flip()
        } else {
          if ("difficulty" %in% names(tables$analysis)) {
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

    output$subdistribution <- renderPlot({
      if (length(tables$subdistribution) > 1) {
        tables$subdistribution %>%
          ggplot2::ggplot(
            ggplot2::aes(x = credits, y = n)
          ) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::xlab("Credits") +
          ggplot2::ylab("Count")
      }
    })

    ############################################################################
    # On exit

    observeEvent(input$done, {
      scores <- scores() %>%
        dplyr::left_join(
          unique(dplyr::select(tables$criteria, question_id, part)),
          by = "question_id"
        ) %>%
        dplyr::select(student_id, question_id, part, score, total)

      WriteXLS::WriteXLS(tables$criteria, "weighted_criteria.xlsx")
      WriteXLS::WriteXLS(scores, "scores.xlsx")
      stopApp()
    })
  }

  runGadget(ui, server, viewer = shiny::browserViewer())
}


add_missing <- function(x) {
  x %>%
    tidyr::pivot_wider(
      names_from = "criterion_id",
      values_from = "grade",
      values_fill = 0
    ) %>%
    tidyr::pivot_longer(
      cols = unique(x$criterion_id),
      names_to = "criterion_id",
      values_to = "grade"
    )
}
