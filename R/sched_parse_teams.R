#' Parse the Team Schedule Workbook to Standardized Format
#'
#' `sched_parse_teams()` parses the data returned from `sched_load_teams()` into
#' a standard format, with one column per team and one row per individual.
#'
#' @param .data The teams data read by `sched_load_teams()`
#'
#' @return A `tibble` containing one column for each team (named with that
#' team's letter designation) and one row per team member. Team member names
#' are standardized with
#' \code{\link[covidassign:sched_std_names]{sched_std_names()}}.
#'
#' @export
sched_parse_teams <- function(.data) {
  .data %>%
    # Find rows with investigator names
    dplyr::mutate(
      row = vec_seq_along(.),
      inv_role = tidyr::replace_na(.data[["role"]] == "Investigators", FALSE),
      inv_start_row = .data[["row"]][.data[["inv_role"]]],
      .before = 1L
    ) %>%
    # Filter to rows with investigator names
    dplyr::filter(.data[["row"]] >= .data[["inv_start_row"]]) %>%
    # Remove helper variables
    dplyr::select(-c("row", "inv_role", "inv_start_row", "role")) %>%
    # Remove empty rows
    janitor::remove_empty(which = "rows") %>%
    # Parse investigator names
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ sched_std_names(.x))
    )
}
