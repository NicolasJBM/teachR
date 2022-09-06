#' @name filter_statistics_server
#' @title Select documents from statistics (server)
#' @author Nicolas Mangin
#' @description Module allowing the user to select documents by clicking on the different sections of a classification statistics.
#' @param id ID of the module to connect the user interface to the appropriate server side.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @return Tibble. List of selected documents.
#' @import shiny
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @export

filter_statistics_server <- function(
  id, course_data
){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {

    type <- NULL
    
    documents <- shiny::reactive({
      shiny::req(!base::is.null(course_data()))
      shiny::req(!base::is.na(course_data()$documents))
      shiny::req(!base::is.na(course_data()$page_ratings))
      shiny::req(!base::is.na(course_data()$video_views))
      shiny::req(!base::is.na(course_data()$document_parameters))
      
      course_data()$documents |>
        dplyr::left_join(course_data()$page_ratings, by = "file") |>
        dplyr::left_join(course_data()$video_views, by = "file") |>
        dplyr::left_join(course_data()$document_parameters, by = "file")
    })
    
    
    ratings_variables <- shiny::reactive({
      teachR::filter_prepare_variables(documents(), "ratings")
    })
    output$ratings_filters <- shiny::renderUI({
      shiny::req(!base::is.null(documents()))
      shiny::req(!base::is.null(ratings_variables()))
      teachR::filter_make_ui(ns, documents(), ratings_variables())
    })
    selected_from_ratings <- shiny::reactive({
      shiny::req(base::nrow(documents()) > 0)
      shiny::req(!base::is.null(ratings_variables()))
      after_ratings <- documents()
      for (i in base::seq_len(base::nrow(ratings_variables()))){
        if (input$applyratingsfilters &
            !base::is.null(input[[ratings_variables()$input_id[i]]])){
          after_ratings <- teachR::filter_tibble(
            dataset = after_ratings,
            variable = ratings_variables()$variable_name[i],
            filter_value = input[[ratings_variables()$input_id[i]]],
            filter_type = ratings_variables()$filter_type[i]
          )
        }
      }
      after_ratings
    })
    
    views_variables <- shiny::reactive({
      teachR::filter_prepare_variables(selected_from_ratings(), "views")
    })
    output$views_filters <- shiny::renderUI({
      shiny::req(!base::is.null(selected_from_ratings()))
      shiny::req(!base::is.null(views_variables()))
      teachR::filter_make_ui(ns, selected_from_ratings(), views_variables())
    })
    selected_from_views <- shiny::reactive({
      shiny::req(base::nrow(selected_from_ratings()) > 0)
      shiny::req(!base::is.null(views_variables()))
      after_views <- selected_from_ratings()
      for (i in base::seq_len(base::nrow(views_variables()))){
        if (input$applyviewsfilters &
            !base::is.null(input[[views_variables()$input_id[i]]])){
          after_views <- teachR::filter_tibble(
            dataset = after_views,
            variable = views_variables()$variable_name[i],
            filter_value = input[[views_variables()$input_id[i]]],
            filter_type = views_variables()$filter_type[i]
          )
        }
      }
      after_views
    })
    
    results_variables <- shiny::reactive({
      teachR::filter_prepare_variables(selected_from_views(), "results")
    })
    output$results_filters <- shiny::renderUI({
      shiny::req(!base::is.null(selected_from_views()))
      shiny::req(!base::is.null(results_variables()))
      teachR::filter_make_ui(ns, selected_from_views(), results_variables())
    })
    selected_from_results <- shiny::reactive({
      shiny::req(base::nrow(selected_from_views()) > 0)
      shiny::req(!base::is.null(results_variables()))
      after_results <- selected_from_views()
      for (i in base::seq_len(base::nrow(results_variables()))){
        if (input$applyresultsfilters &
            !base::is.null(input[[results_variables()$input_id[i]]])){
          after_results <- teachR::filter_tibble(
            dataset = after_results,
            variable = results_variables()$variable_name[i],
            filter_value = input[[results_variables()$input_id[i]]],
            filter_type = results_variables()$filter_type[i]
          )
        }
      }
      dplyr::select(after_results, file)
    })
    
    return(selected_from_results)
  })
}

