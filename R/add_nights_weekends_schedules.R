#' Join Nights and Weekends Schedules to Teams Data
#'
#' `asg_add_nights_weekends_schedules()` loads individual schedules for
#' investigators on night or weekend shifts and joins them with the output of
#' `asg_join_schedules()`.
#'
#' @inheritParams asg_load_nights_weekends
#'
#' @param .data The output of `as_join_schedules()`
#'
#' @return A `tibble` with a `schedule_custom` column added
#'
#' @family Case Assignment
#'
#' @export
asg_add_nights_weekends_schedules <- function(
  .data,
  path = path_create(
    "V:/Administration/Schedules/Night&Weekend staff",
    ext = "xlsx"
  ),
  sheet = "schedules"
) {

  nights_weekends_schedules <- path %>%
    asg_load_nights_weekends(
      sheet = sheet,
      clean_names = TRUE
    ) %>%
    asg_parse_nights_weekends()

  nights_weekends_teams <- .data %>%
    dplyr::filter(.data[["schedule"]] == "nights-weekends")

  # Join nights-weekends only for more informative message
  dplyr::left_join(
    nights_weekends_teams,
    nights_weekends_schedules,
    by = "member",
    suffix = c("", "_custom")
  )

  # Join full data for output
  .data %>%
    dplyr::left_join(
      nights_weekends_schedules,
      by = "member",
      suffix = c("", "_custom")
    ) %>%
    dplyr::rename(cycle = .data[["schedule_custom"]])
}
