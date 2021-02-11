#' Get Investigators Scheduled to Work on a Given Date
#'
#' `sched_investigators()` parses scheduling files and returns
#' investigators scheduled to work on `date`.
#'
#' @inheritParams sched_calc
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
sched_investigators <- function(
  date = lubridate::today(),
  team_sched = TRUE,
  scheduled_only = TRUE
) {
  rlang::inform("Loading teams...")
  if (team_sched) {
    from_team_sched <- sched_load_teams(path = path_teams()) %>%
      sched_parse_teams() %>%
      sched_join_schedules(schedules = covidassign::team_schedules) %>%
      sched_add_nights_weekends(path = path_nights_weekends()) %>%
      dplyr::mutate(
        null_cycle = purrr::map_lgl(.data[["cycle"]], ~ is.null(.x))
      ) %>%
      # Get rid of investigators with no schedule
      dplyr::filter(
        !(.data[["schedule"]] == "nights-weekends" & .data[["null_cycle"]])
      ) %>%
      dplyr::select(-"null_cycle") %>%
      sched_calc(date = date) %>%
      dplyr::rename(investigator = .data[["member"]])

    sched <- tidylog::left_join(
      sched_load_tomorrow_all(),
      from_team_sched,
      by = c("team", "investigator"),
      suffix = c("", "_from_team_sched")
    )
  } else {
    sched <- sched_load_tomorrow_all()
  }

  sched %>%
    dplyr::transmute(
      .data[["team"]],
      .data[["investigator"]],
      scheduled = coviData::coalesce_across(dplyr::starts_with("scheduled"))
    ) %>%
    purrr::when(
      scheduled_only ~ dplyr::filter(., .data[["scheduled"]]),
      ~ .
    )
}
