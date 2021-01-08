#' Parse Weekdays to Match Full Weekday Names
#'
#' `parse_weekday()` takes a vector of weekday abbreviations and returns the
#' full name of each weekday. If the abbreviation is one letter, it matches the
#' standard one-letter weekday abbreviations (U/M/T/W/R/F/S). If more than one
#' letter, it matches the first two letters to the full day name.
#'
#' @param day A character vector of day names or abbreviations
#'
#' @return A character vector of full day names
#'
#' @keywords internal
#'
#' @export
parse_weekday <- function(day) {

  vctrs::vec_assert(day, ptype = character())

  day <- std_names(day)

  day_len_1 <- stringr::str_length(day) == 1L

  dplyr::case_when(
    day_len_1 & day == "U" ~ "Sunday",
    day_len_1 & day == "M" ~ "Monday",
    day_len_1 & day == "T" ~ "Tuesday",
    day_len_1 & day == "W" ~ "Wednesday",
    day_len_1 & day == "R" ~ "Thursday",
    day_len_1 & day == "F" ~ "Friday",
    day_len_1 & day == "S" ~ "Saturday",
    stringr::str_starts(day, "Su") ~ "Sunday",
    stringr::str_starts(day, "Mo") ~ "Monday",
    stringr::str_starts(day, "Tu") ~ "Tuesday",
    stringr::str_starts(day, "We") ~ "Wednesday",
    stringr::str_starts(day, "Th") ~ "Thursday",
    stringr::str_starts(day, "Fr") ~ "Friday",
    stringr::str_starts(day, "Sa") ~ "Saturday"
  )
}
