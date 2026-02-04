#' @name filter_tree_server
#' @title Select documents from tree
#' @author Nicolas Mangin
#' @description Module allowing the user to select documents by clicking on the different sections of a classification tree.
#' @param id ID of the module to connect the user interface to the appropriate server side.
#' @param jstree Reactive. jstree selected for filtering documents.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @return Tibble. List of selected documents.
#' @importFrom classR trees_selected_json_to_tibble
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom jsTreeR jstree
#' @importFrom jsTreeR jstreeDestroy
#' @importFrom jsTreeR renderJstree
#' @importFrom shiny NS
#' @importFrom shiny moduleServer
#' @importFrom shiny reactive
#' @importFrom shiny req
#' @export

filter_tree_server <- function(id, jstree, course_data){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {

    path <- NULL
    other_languages <- NULL
    type <- NULL
    position <- NULL
    folder <- NULL
    
    output$selection_tree <- jsTreeR::renderJstree({
      shiny::req(base::length(jstree()) >= 2)
      jsTreeR::jstreeDestroy(session, ns("selection_tree"))
      jsTreeR::jstree(
        nodes = jstree(),
        selectLeavesOnly = FALSE,
        checkboxes = TRUE,
        search = FALSE,
        searchtime = 250,
        dragAndDrop = FALSE,
        dnd = NULL,
        multiple = TRUE,
        types = NULL,
        sort = FALSE,
        unique = FALSE,
        wholerow = TRUE,
        contextMenu = FALSE,
        checkCallback = NULL,
        grid = NULL,
        theme = "default"
      ) |> base::suppressMessages()
    })
    
    selected_from_tree <- shiny::reactive({
      shiny::req(base::length(jstree()) >= 2)
      shiny::req(!base::is.na(course_data()$documents))
      if (base::length(input$selection_tree_selected)>0){
        selection <- input$selection_tree_selected
        selection <- classR::trees_selected_json_to_tibble(selection) |>
          dplyr::select(file) |>
          stats::na.omit()
        if (base::length(selection) == 0){
          selection <- course_data()$documents |>
            dplyr::select(file) 
        }
      } else {
        selection <- course_data()$documents |>
          dplyr::select(file) 
      }
      
      if (base::length(jstree) > 1){
        selection <- jstree |>
          dplyr::filter(file %in% selection$file) |>
          dplyr::arrange(position) |>
          dplyr::select(file) |>
          dplyr::left_join(selection, by = "file")
      }
      
      selection
    })
    
    return(selected_from_tree)
  })
}

