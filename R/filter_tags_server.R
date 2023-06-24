#' @name filter_tags_server
#' @title Select documents from tags (server)
#' @author Nicolas Mangin
#' @description Module allowing the user to select documents by clicking on the different sections of a classification tags.
#' @param id ID of the module to connect the user interface to the appropriate server side.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @return Tibble. List of selected documents.
#' @importFrom dplyr filter
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny reactive
#' @importFrom shiny renderText
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @export


filter_tags_server <- function(id, course_data){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {

    type <- NULL
    
    tags <- shiny::reactive({
      course_data()$tags
    })
    
    common_tags_variables <- shiny::reactive({
      shiny::req(!base::is.null(tags()))
      shiny::req(base::length(tags()) > 1)
      teachR::filter_prepare_variables(
        course_data()$documents, "common_tags", tags()
      )
    })
    
    output$commontags_filters <- shiny::renderUI({
      shiny::req(!base::is.null(course_data()$documents))
      shiny::req(!base::is.na(course_data()$documents))
      shiny::req(!base::is.null(common_tags_variables()))
      teachR::filter_make_ui(
        ns, course_data()$documents, common_tags_variables()
      )
    })
    
    selected_from_common_tags <- shiny::reactive({
      shiny::req(base::nrow(course_data()$documents) > 0)
      shiny::req(!base::is.null(common_tags_variables()))
      after_common_tags <- course_data()$documents
      for (i in base::seq_len(base::nrow(common_tags_variables()))){
        if (!base::is.null(input[[common_tags_variables()$input_id[[i]]]]) &
            base::length(input[[common_tags_variables()$input_id[[i]]]]) > 0){
          after_common_tags <- teachR::filter_tibble(
            dataset = after_common_tags,
            variable = common_tags_variables()$variable_name[[i]],
            filter_value = input[[common_tags_variables()$input_id[[i]]]],
            filter_type = common_tags_variables()$filter_type[[i]]
          )
        } else after_common_tags <- after_common_tags
      }
      after_common_tags
    })
    
    custom_tags_variables <- shiny::reactive({
      teachR::filter_prepare_variables(
        selected_from_common_tags(), "custom_tags", tags()
      )
    })
    
    output$customtags_filters <- shiny::renderUI({
      shiny::req(!base::is.null(selected_from_common_tags()))
      shiny::req(!base::is.null(custom_tags_variables()))
      teachR::filter_make_ui(
        ns, selected_from_common_tags(), custom_tags_variables(), tags()
      )
    })
    
    selected_from_custom_tags <- shiny::reactive({
      shiny::req(base::nrow(selected_from_common_tags()) > 0)
      shiny::req(!base::is.null(custom_tags_variables()))
      after_custom_tags <- selected_from_common_tags()
      for (i in base::seq_len(base::nrow(custom_tags_variables()))){
        if (!base::is.null(input[[custom_tags_variables()$input_id[i]]])){
          after_custom_tags <- teachR::filter_tibble(
            dataset = after_custom_tags,
            variable = custom_tags_variables()$variable_name[i],
            filter_value = input[[custom_tags_variables()$input_id[i]]],
            filter_type = custom_tags_variables()$filter_type[i]
          )
        } else after_custom_tags <- after_custom_tags
      }
      dplyr::select(after_custom_tags, file)
    })
    
    return(selected_from_custom_tags)
  })
}
