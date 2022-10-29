#' @name filter_languages_server
#' @title Filter documents based on languages
#' @author Nicolas Mangin
#' @description Module allowing the user to find select document based on whether they exist in other languages.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @return Tibble. list of documents existing in all the selected languages.
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny reactive
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny tagList
#' @importFrom shinyWidgets multiInput
#' @importFrom stats na.omit
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @export



filter_languages_server <- function(id, course_data){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    language <- NULL
    langiso <- NULL
    translations <- NULL
    type <- NULL
    
    documents <- shiny::reactive({
      shiny::req(!base::is.na(course_data()$documents))
      course_data()$documents
    })
    
    languages <- shiny::reactive({
      shiny::req(!base::is.na(course_data()$languages))
      main_language <- base::unique(documents()$language)[[1]]
      other_languages <- documents()$translations |>
        stringr::str_split(pattern = " ", simplify = TRUE) |>
        base::unlist() |> stats::na.omit()
      course_data()$languages |>
        dplyr::mutate(
          type = dplyr::case_when(
            langiso == main_language ~ "original",
            langiso %in% other_languages ~ "translation",
            TRUE ~ "missing"
          )
        )
    })
    
    output$filtexistinglang <- shiny::renderUI({
      shiny::req(!base::is.na(course_data()$languages))
      shiny::req(!base::is.na(languages()))
      existing <- languages() |>
        dplyr::filter(type == "translation")
      shinyWidgets::multiInput(
        inputId = ns("slctexistlanguages"),
        label = "Select the other language(s) in which documents should already be translated:", 
        choices = NULL,
        choiceNames = existing$language,
        choiceValues = existing$langiso,
        width = "100%"
      )
    })
    
    output$filtmissinglang <- shiny::renderUI({
      shiny::req(!base::is.na(course_data()$languages))
      shiny::req(!base::is.na(languages()))
      missing <- languages() |>
        dplyr::filter(type != "original")
      shinyWidgets::multiInput(
        inputId = ns("slctmisslanguages"),
        label = "Select the other language(s) in which documents should not be translated yet:", 
        choices = NULL,
        choiceNames = missing$language,
        choiceValues = missing$langiso,
        width = "100%"
      )
    })
    
    selcted_from_languages <- shiny::reactive({
      shiny::req(!base::is.na(course_data()$languages))
      shiny::req(base::nrow(documents()) > 0)
      after_languages <- documents()
      if (!base::is.null(input$slctexistlanguages)){
        for (exist in input$slctexistlanguages){
          after_languages <- after_languages |>
            dplyr::filter(stringr::str_detect(translations, exist))
        }
      }
      if (!base::is.null(input$slctmisslanguages)){
        for (miss in input$slctmisslanguages){
          after_languages <- after_languages |>
            dplyr::filter(!stringr::str_detect(translations, miss))
        }
      }
      dplyr::select(after_languages, file)
    })
    
    
    return(selcted_from_languages)
  })
}

