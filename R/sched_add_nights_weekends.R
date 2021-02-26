#' Join Nights and Weekends Schedules to Teams Data
#'
#' `sched_add_nights_weekends()` loads individual schedules for
#' investigators on night or weekend shifts and joins them with the output of
#' `sched_join_schedules()`.
#'
#' @inheritParams sched_load_nights_weekends
#'
#' @param .data The output of `sched_join_schedules()`
#'
#' @return A `tibble` with a `schedule_custom` column added
#'
#' @family Case Assignment
#'
#' @export
sched_add_nights_weekends <- function(
  .data,
  path = path_nights_weekends(),
  sheet = "schedules"
) {

  nights_weekends_schedules <- path %>%
    sched_load_nights_weekends(
      sheet = sheet,
      clean_names = TRUE
    ) %>%
    sched_parse_nights_weekends()

  nights_weekends_teams <- .data %>%
    dplyr::filter(.data[["schedule"]] == "nights-weekends")

  # Join full data for output
  .data %>%
    dplyr::left_join(
      nights_weekends_schedules,
      by = "member",
      suffix = c("", "_nights_weekends")
    ) %>%
    dplyr::rename(cycle = .data[["schedule_nights_weekends"]])
}
