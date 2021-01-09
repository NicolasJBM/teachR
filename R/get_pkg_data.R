#' @name get_pkg_data
#' @title Retrieve data from packages
#' @author Nicolas Mangin
#' @description Get the requested data from a specified package.
#' @param pkgname Character. Name of the package containing the data.
#' @param dataset Character. Name of the data to be retrieved from this package
#' @return data
#' @export

# Function to retreive str_question from a package
get_pkg_data <- function(pkgname, dataset) {
  eval(parse(text = paste0(pkgname, "::", dataset)))
}
