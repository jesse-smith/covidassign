#' Team Schedules Used for Case Assignment
#'
#' A dataset containing the case investigation teams, the schedule followed by
#' each team, and the anchor date for the rotating schedules.
#'
#' @format A data frame with 7 rows and 3 variables:
#'   \describe{
#'     \item{team}{a character vector of investigation team letter designations}
#'     \item{schedule}{a character vector of schedule types}
#'     \item{anchor}{a `Date` vector of anchor dates for rotating schedules}
#'   }
#'
#' @source "Investigation Shift Schedules.xlsx"
"team_schedules"
