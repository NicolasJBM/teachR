#' @name filter_make_ui
#' @title Make filters
#' @author Nicolas Mangin
#' @description Function creating user interfaces to select documents based on specified variables
#' @param ns Function. Apply the ID of the module in which the function is embedded.
#' @param preselection Tibble
#' @param filter_variables Tibble. Output of the function filter_prepare_variables.
#' @param tags Tibble. List of tags.
#' @return A user interface with adequate filters for each variable.
#' @seealso filter_prepare_variables
#' @seealso filter_tibble
#' @importFrom dplyr arrange
#' @importFrom shiny checkboxInput
#' @importFrom shiny icon
#' @importFrom shiny numericInput
#' @importFrom shiny selectInput
#' @importFrom shiny sliderInput
#' @importFrom shinyWidgets checkboxGroupButtons
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyWidgets searchInput
#' @export



filter_make_ui <- function(ns, preselection, filter_variables, tags = NA){
  
  filters <- base::list()
  
  if (base::length(tags) > 1){
    tags <- tags |>
      dplyr::arrange(tag, order)
    tagvalues <- tags$value
    tagvalues <- tagvalues[tagvalues != ""]
  } else {
    tagvalues <- NA
  }
  
  for (i in base::seq_len(base::nrow(filter_variables))){
    
    vartype <- filter_variables$filter_type[i]
    variable <- filter_variables$variable_name[i]
    varvalues <- preselection[,variable] |>
      base::lapply(stringr::str_split, pattern = " ", simplify = TRUE) |>
      base::unlist() |>
      base::unique()
    
    if (variable == "type"){
      varvalues <- varvalues |>
        base::factor(levels = c(
          "Presentation", "Script", "Page", "Paper", 
          "Free", "Statements", "Alternatives",
          "Computation", "Essay", "Problem"
        )) |>
        base::sort() |> base::as.character()
    } else if (!base::is.na(tagvalues[1])){
      varvalues <- base::intersect(tagvalues, varvalues)
    } else varvalues <- base::sort(varvalues)
    varvalues <- varvalues[varvalues != ""]
    inputid <- ns(filter_variables$input_id[i])
    
    if (vartype == "logical") {
      
      filters[[i]] <- shiny::checkboxInput(
        inputid,
        variable,
        value = FALSE,
        width = "100%"
      )
      
    } else if (vartype %in% c("selection", "multiple")) {
      
      if (base::length(varvalues) > 15){
        filters[[i]] <- shiny::selectInput(
          inputid,
          variable,
          choices = c("",varvalues),
          selected = "",
          width = "100%",
          multiple = TRUE
        )
      } else {
        filters[[i]] <- shinyWidgets::checkboxGroupButtons(
          inputId = inputid, 
          label = variable,
          choices = varvalues,
          selected = base::character(0),
          status = "success",
          justified = FALSE,
          direction = "horizontal",
          size = "normal",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      }
      
    } else if (vartype == "pattern") {
      
      filters[[i]] <- shinyWidgets::searchInput(
        inputId = inputid, label = variable,
        width = "100%", placeholder = "Pattern to search",
        btnSearch = shiny::icon("magnifying-glass"), btnReset = shiny::icon("eraser")
      )
      
    } else if (vartype == "range") {
      
      varvalues <- stats::na.omit(base::as.numeric(varvalues))
      varvalues <- varvalues[base::is.finite(varvalues)]
      
      if (base::length(varvalues) > 1){
        minimum <- base::min(varvalues)
        maximum <- base::max(varvalues)
      } else {
        minimum <- 0
        maximum <- 1
      }
      
      filters[[i]] <- shiny::sliderInput(
        inputid,
        variable,
        min = minimum,
        max = maximum,
        value = c(minimum,maximum),
        width = "100%"
      )
      
    } else { # so "value"
      
      filters[[i]] <- shiny::numericInput(
        inputid,
        variable,
        value = NA,
        width = "100%"
      )
      
    }
    
  }
  
  return(filters)
}



