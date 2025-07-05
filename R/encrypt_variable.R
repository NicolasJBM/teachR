#' @name encrypt_variable
#' @title Encrypt a variable
#' @author Nicolas Mangin
#' @description Encrypt the specified variable of a data frame using the specified key
#' @param x Tibble. Data in with the variable to be encrypted
#' @param var Character. Name of the variable which should be encrypted
#' @param key Character. String used to encrypt the variable
#' @return Tibble. Initial tibble in which the specified variable is encrypted
#' @importFrom purrr map_chr
#' @importFrom stringr str_replace_all
#' @importFrom cryptography autokey
#' @export


encrypt_variable <- function(x, var, key){
  y <- x[,var] |>
    stringr::str_replace_all("\\-", "symbdash") |>
    stringr::str_replace_all("\\=", "symbequal") |>
    stringr::str_replace_all("\\?", "symbquestion") |>
    stringr::str_replace_all("\\_", "symbunderscore") |>
    stringr::str_replace_all("\\:", "symbcolon") |>
    stringr::str_replace_all("\\/", "symbslash") |>
    stringr::str_replace_all("\\.", "symbpoint") |>
    stringr::str_replace_all("\\&", "symbesperl") |>
    stringr::str_replace_all("\\@", "symbat") |>
    purrr::map_chr(cryptography::autokey, key = key, encrypt = TRUE)
  x[,var] <- y
  return(x)
}

