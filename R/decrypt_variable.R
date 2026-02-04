#' @name decrypt_variable
#' @title Decrypt a variable
#' @author Nicolas Mangin
#' @description Decrypt the specified variable of a data frame using the specified key
#' @param x Tibble. Data in with the variable to be decrypted
#' @param var Character. Name of the variable which should be decrypted
#' @param key Character. String used to decrypt the variable
#' @return Tibble. Initial tibble in which the specified variable is decrypted
#' @importFrom purrr map_chr
#' @importFrom stringr str_replace_all
#' @importFrom cryptography autokey
#' @export


decrypt_variable <- function(x, var, key){
  y <- x[,var] |>
    base::unlist() |>
    base::as.character() |>
    purrr::map_chr(cryptography::autokey, key = key, encrypt = FALSE) |>
    stringr::str_replace_all("symbdash", "\\-") |>
    stringr::str_replace_all("symbequal", "\\=") |>
    stringr::str_replace_all("symbquestion", "\\?") |>
    stringr::str_replace_all("symbunderscore", "\\_") |>
    stringr::str_replace_all("symbcolon", "\\:") |>
    stringr::str_replace_all("symbslash", "\\/") |>
    stringr::str_replace_all("symbpoint", "\\.") |>
    stringr::str_replace_all("symbesperl", "\\&") |>
    stringr::str_replace_all("symbat", "\\@")
  x[,var] <- y
  return(x)
}
