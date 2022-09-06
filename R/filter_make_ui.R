#' @name filter_make_ui
#' @title Create filters UI
#' @author Nicolas Mangin
#' @description Function creating user interfaces to select documents based on specified variables
#' @param ns Function. Apply the ID of the module in which the function is embedded.
#' @param preselection Tibble
#' @param filter_variables Tibble. Output of the function filter_prepare_variables.
#' @param tags Tibble. List of tags.
#' @return A user interface with adequate filters for each variable.
#' @seealso filter_prepare_variables
#' @seealso filter_tibble
#' @importFrom shiny checkboxInput
#' @importFrom shiny selectInput
#' @importFrom shiny textInput
#' @importFrom shiny sliderInput
#' @importFrom shiny numericInput
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
    varvalues <- base::unique(base::unlist(preselection[,variable]))
    if (!base::is.na(tagvalues[1])){
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
      
    } else if (vartype == "selection") {
      
      filters[[i]] <- shiny::selectInput(
        inputid,
        variable,
        choices = c("", varvalues),
        selected = "",
        width = "100%",
        multiple = FALSE
      )
      
    } else if (vartype == "multiple") {
      
      filters[[i]] <- shiny::selectInput(
        inputid,
        variable,
        choices = c("", varvalues),
        selected = "",
        width = "100%",
        multiple = TRUE
      )
      
    } else if (vartype == "pattern") {
      
      filters[[i]] <- shinyWidgets::searchInput(
        inputId = inputid, label = variable,
        width = "100%", placeholder = "Pattern to search",
        btnSearch = shiny::icon("magnifying-glass"), btnReset = shiny::icon("eraser")
      )
      
    } else if (vartype == "range") {
      
      minimum <- base::suppressWarnings(base::min(varvalues, na.rm = TRUE))
      if (!base::is.finite(minimum)) minimum <- 0
      maximum <- base::suppressWarnings(base::max(varvalues, na.rm = TRUE))
      if (!base::is.finite(maximum)) maximum <- 1
      
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



