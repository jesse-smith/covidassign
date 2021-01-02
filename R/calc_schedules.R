#' Calculate Whether Investigators Are Working on a Given Date
#'
#' `asg_calc_schedules()` takes the output of
#' `asg_add_nights_weekends_schedules()` and calcuates whether investigators are
#' scheduled to work on `date`. Either a predefined schedule or a custom
#' schedule must be supplied for every investigator.
#'
#' @param .data The output of `asg_join_nights_weekends()`
#'
#' @param date The date to use when checking whether investigators are scheduled
#'
#' @return A `tibble` with columns `team`, `member`, and `scheduled`
#'
#' @family Case Assignment
#'
#' @export
asg_calc_schedules <- function(.data, date = Sys.Date()) {
  # Convert to arguments expected by `asg_schedule_predefined()`
  data_predefined <- .data %>%
    dplyr::mutate(
      schedule_predefined = .data[["schedule"]] %>%
        stringr::str_remove_all(pattern = "-") %>%
        stringr::str_replace_all(
          pattern = "nightsweekends",
          replacement = "custom"
        )
    )

  data_predefined %>%
    dplyr::mutate(
      scheduled = data_predefined %>%
        dplyr::select("schedule_predefined", "anchor", "cycle") %>%
        purrr::pmap_lgl(
          ~ asg_schedule_predefined(
            schedule = ..1,
            start = date,
            end = date + 1L,
            anchor = ..2,
            cycle = ..3
          ) %>%
            dplyr::pull("scheduled") %>%
            extract2(1L)
        )
    ) %>%
    dplyr::select("team", "member", "scheduled")
}
