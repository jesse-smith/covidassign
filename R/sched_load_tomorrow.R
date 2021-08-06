#' Load All Schedules for Tomorrow Provided by Team Leads
#'
#' `sched_load_tomorrow_all()` loads all schedules provided by team leads.
#'
#' @param dir Character. Path to directory containing excel file schedules.
#'
#' @return A `tibble` with columns `investigator` (`character`) and
#'   `scheduled` (`logical`)
#'
#' @export
sched_load_tomorrow_all <- function(
  dir = "V:/Investigation Team Leads/Daily Assignment Schedule/"
) {
  path_create(dir) %>%
    fs::dir_ls(type = "file", regexp = ".*/[^/~]+[.]xlsx$") %>%
    purrr::map_dfr(sched_load_tomorrow)
}


#' Load Schedules for Tomorrow Provided by Team Leads
#'
#' `sched_load_tomorrow()` loads schedules for the following day filled out
#' by team leads.
#'
#' @param path Character. Path to the excel file containing schedules.
#'
#' @return A `tibble` with columns `investigator` (`character`) and
#'   `scheduled` (`logical`)
#'
#' @export
sched_load_tomorrow <- function(path) {

  team <- path_create(path) %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    stringr::str_to_upper() %>%
    stringr::str_remove_all("[^A-Z ]+") %>%
    stringr::str_squish() %>%
    stringr::str_extract("(?<=[ ])[A-Z]$|SCHOOL") %>%
    stringr::str_to_lower()

  suppressMessages(coviData::read_file_excel(path)) %>%
    dplyr::select(investigator = 1L, scheduled = 2L) %>%
    janitor::remove_empty("rows") %>%
    dplyr::mutate(
      team = {{ team }},
      investigator = sched_std_names(.data[["investigator"]]),
      scheduled = .data[["scheduled"]] == "Yes"
    ) %>%
    dplyr::relocate("team", "investigator", "scheduled")
}
