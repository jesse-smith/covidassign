#' Standardize Addresses
#'
#' `std_addr()` transliterates all characters to ASCII, replaces all symbols
#' except dashes (`-`) and apostrophes (`'`) with a space, removes apostrophes,
#' removes extraneous whitespace, and converts to title case.
#'
#' @param x Character-like vector. This will be converted to character before
#'   standardizing.
#'
#' @return A character vector with standardized addresses
#'
#' @export
std_addr <- function(x) {
  x %>%
    as.character() %>%
    stringi::stri_trans_general(id = "Any-Latin;Latin-ASCII") %>%
    stringr::str_replace_all(
      pattern = "[^a-zA-Z0-9-']",
      replacement = " "
    ) %>%
    stringr::str_replace_all(pattern = "[']", replacement = "") %>%
    stringr::str_replace_all(
      pattern = "[\t\n\r ]*-[\t\n\r ]*",
      replacement = "-"
    ) %>%
    stringr::str_squish() %>%
    stringr::str_to_upper()
}
