#' @name filter_tree_server
#' @title Select documents from tree (server)
#' @author Nicolas Mangin
#' @description Module allowing the user to select documents by clicking on the different sections of a classification tree.
#' @param id ID of the module to connect the user interface to the appropriate server side.
#' @param tree Reactive. Function containing a list of documents as a classification tree compatible with jsTreeR
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @return Tibble. List of selected documents.
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom jsTreeR jstree
#' @importFrom jsTreeR jstreeDestroy
#' @importFrom jsTreeR renderJstree
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny reactive
#' @importFrom shiny req
#' @importFrom stats na.omit
#' @export

filter_tree_server <- function(
  id, tree, course_data
){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {

    path <- NULL
    other_languages <- NULL
    type <- NULL
    position <- NULL
    folder <- NULL
    
    output$selection_tree <- jsTreeR::renderJstree({
      shiny::req(!base::is.na(tree()))
      jsTreeR::jstreeDestroy(session, ns("selection_tree"))
      jsTreeR::jstree(
        nodes = tree()$jstree,
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
      
      if (base::length(tree()$tbltree) > 1){
        
        selection1 <- tree()$textbook |>
          dplyr::filter(file %in% selection$file) |>
          dplyr::arrange(folder) |>
          dplyr::select(file) |>
          dplyr::left_join(selection, by = "file")
        
        selection2 <- tree()$tbltree |>
          dplyr::anti_join(selection1, by = "file") |>
          dplyr::filter(file %in% selection$file) |>
          dplyr::arrange(position) |>
          dplyr::select(file) |>
          dplyr::left_join(selection, by = "file")
        
        selection <- dplyr::bind_rows(selection1, selection2)
        
      }
      
      selection
    })
    
    return(selected_from_tree)
  })
}

