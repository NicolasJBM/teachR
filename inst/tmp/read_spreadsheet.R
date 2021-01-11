#' @name read_spreadsheet
#' @title Read Spreadsheets
#' @author Nicolas Mangin
#' @description Recognize the type of spreadsheet based on its extension and import it.
#' @param path Character. Path to the spreadsheet.
#' @return A dataframe
#' @importFrom stringr str_detect
#' @importFrom dplyr case_when
#' @importFrom readxl read_excel
#' @importFrom readODS read_ods
#' @importFrom readr read_csv
#' @export

read_spreadsheet <- function(path) {
  
  stopifnot(
    stringr::str_detect(path, ".csv$|.xlsx$|.ods$")
  )
  
  table <- dplyr::case_when(
    stringr::str_detect(path, ".csv$") ~
      readr::read_csv(path),
    stringr::str_detect(path, ".xlsx$") ~
      readxl::read_excel(path),
    TRUE ~
      readODS::read_ods(path)
  )
  
  return(table)
}