#' @name filter_documents_count
#' @title Count the different kinds of selected documents.
#' @author Nicolas Mangin
#' @description Function reporting the number of documents of each kind after applying filters.
#' @param selection Tibble. Filtered documents.
#' @return Character. String reporting the number of each document after filter application.
#' @importFrom dplyr filter
#' @export


filter_documents_count <- function(selection){
  
  type <- NULL
  
  docnbr <- selection |>
    base::nrow()
  slidenbr <- selection |>
    dplyr::filter(type == "Presentation") |>
    base::nrow()
  videonbr <- selection |>
    dplyr::filter(type == "Script") |>
    base::nrow()
  pagenbr <- selection |>
    dplyr::filter(type == "Page") |>
    base::nrow()
  notenbr <- selection |>
    dplyr::filter(type == "Paper") |>
    base::nrow()
  statementnbr <- selection |>
    dplyr::filter(type == "Statements") |>
    base::nrow()
  alternativenbr <- selection |>
    dplyr::filter(type == "Alternatives") |>
    base::nrow()
  computationnbr <- selection |>
    dplyr::filter(type == "Computation") |>
    base::nrow()
  essaynbr <- selection |>
    dplyr::filter(type == "Essay") |>
    base::nrow()
  problemnbr <- selection |>
    dplyr::filter(type == "Problem") |>
    base::nrow()
  base::paste0(
    docnbr,
    " selected documents: ",
    slidenbr, " presentation(s), ",
    videonbr, " script(s), ",
    pagenbr, " page(s), ",
    notenbr, " paper(s), ",
    statementnbr, " statements question(s), ",
    alternativenbr, " alternatives question(s), ",
    computationnbr, " computation question(s), ",
    essaynbr, " essay question(s), and ",
    problemnbr, " problem question(s), "
  )
}
