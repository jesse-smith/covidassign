#' Join the `team_schedules` Dataset to Teams
#'
#' `sched_join_schedules()` pivots the `teams` data to long format and joins the
#' schedules specified in \code{\link{team_schedules}} to the result.
#'
#' @param teams A data frame containing teams in columns and members in rows;
#'   designed to take the output of
#'   \code{\link[covidassign:sched_parse_teams]{sched_parse_teams()}}
#'
#' @param schedules Schedules for each team, given by the `team_schedules`
#'   dataset
#'
#' @return A `tibble` with columns for `team`, `member`, `schedule`, and
#'   `anchor`
#'
#' @family Case Assignment
#'
#' @export
sched_join_schedules <- function(
  teams,
  schedules = covidassign::team_schedules
) {
  teams %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = "team",
      values_to = "member"
    ) %>%
    stats::na.omit() %>%
    dplyr::arrange(.data[["team"]], .data[["member"]]) %>%
    dplyr::left_join(
      covidassign::team_schedules,
      by = "team"
    )
}
