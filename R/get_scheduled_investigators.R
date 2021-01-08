#' Get Investigators Scheduled to Work on a Given Date
#'
#' `get_scheduled_investigators()` parses scheduling files and returns
#' investigators scheduled to work on `date`.
#'
#' @inheritParams calc_schedules
#'
#' @param path_teams The location of the teams scheduling Excel workbook
#'   data
#'
#' @param path_nights_weekends The location of the nights and weekends
#'   scheduling Excel workbook
#'
#' @param team_schedules A `tibble` containing information for parsing schedules
#'   into dates; the default is the built-in `team_schedules` dataset and should
#'   probably not be changed
#'
#' @param scheduled_only Should only scheduled investigators be returned? If
#'   `FALSE`, all investigators will be returned. Useful for debugging.
#'
#' @return A `tibble` containing `team`, `investigator`, and `scheduled` columns
#'
#' @export
get_scheduled_investigators <- function(
  date = Sys.Date(),
  path_teams = path_create(
    "V:/Administration/Schedules/Investigation Staff Schedule",
    ext = "xlsx"
  ),
  path_nights_weekends = path_create(
    "V:/Administration/Schedules/Night&Weekend staff",
    ext = "xlsx"
  ),
  team_schedules = team_schedules,
  scheduled_only = TRUE
) {
  rlang::inform("Loading teams...")
  load_teams(path = path_teams) %>%
    parse_teams() %>%
    join_schedules(schedules = team_schedules) %>%
    add_nights_weekends_schedules(path = path_nights_weekends) %>%
    dplyr::mutate(
      null_cycle = purrr::map_lgl(.data[["cycle"]], ~ is.null(.x))
    ) %>%
    # Get rid of investigators with no schedule
    dplyr::filter(
      !(.data[["schedule"]] == "nights-weekends" & .data[["null_cycle"]])
    ) %>%
    dplyr::select(-"null_cycle") %>%
    calc_schedules(date = date) %>%
    dplyr::rename(investigator = .data[["member"]]) %>%
    purrr::when(
      scheduled_only ~ dplyr::filter(., .data[["scheduled"]]),
      ~ .
    )
}
