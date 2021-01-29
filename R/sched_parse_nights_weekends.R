#' Parse and Re-Shape Nights and Weekends Scheduling Data
#'
#' `sched_parse_nights_weekends()` parses night/weekend schedules into a standard
#' format for case assignment.
#'
#' @param .data Data read by
#'   \code{
#'   \link[covidassign:sched_load_nights_weekends]{sched_load_nights_weekends()}
#'   }
#'
#' @return A `tibble` with one row per investigator and columns named `member`
#'   and `schedule`; the latter is a list-column containing named logical
#'   vectors of the weekly schedules for each investigator
#'
#' @family Case Assignment
#'
#' @export
sched_parse_nights_weekends <- function(.data) {
  .data %>%
    dplyr::mutate(role = sched_std_names(.data[["role"]])) %>%
    dplyr::filter(.data[["role"]] == "Investigator") %>%
    dplyr::select(-c("role", "schedule", "notes")) %>%
    dplyr::mutate(
      member = sched_std_names(.data[["member"]]),
      dplyr::across(!"member", ~ !is.na(.x))
    ) %>%
    tidyr::pivot_longer(
      !"member",
      names_to = "weekday",
      values_to = "scheduled"
    ) %>%
    dplyr::mutate(weekday = sched_parse_weekday(.data[["weekday"]])) %>%
    dplyr::group_by(.data[["member"]]) %>%
    dplyr::summarize(
      schedule = list(
        schedule = set_names(.data[["scheduled"]], .data[["weekday"]])
      )
    )
}
