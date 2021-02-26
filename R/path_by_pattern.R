#' Get Path(s) to the Most Recent Scheduling Files
#'
#' @description
#' `path_teams()` finds the path to the most recent investigation teams
#' schedule.
#'
#' `path_nights_weekends()` finds the path to the most recent nights/weekends
#' schedule.
#'
#' @details
#' These functions returns the most recently created file; if multiple files are
#' create within 1 second of each other, this will return a vector of paths to
#' each of those files.
#'
#' @inheritParams coviData::path_by_pattern
#'
#' @return An `fs_path` vector
#'
#' @name covidassign-paths
#'
#' @aliases path_teams path_nights_weekends
NULL

#' @rdname covidassign-paths
#'
#' @export
path_teams <- function(
  dir = "V:/Administration/Schedules/",
  pattern = "(?i).*/[^/~]*Investigations?[ ]*Staff[ ]*Schedule.*xlsx?$"
) {
  path_by_pattern(dir = dir, pattern = pattern, by = "created")
}

#' @rdname covidassign-paths
#'
#' @export
path_nights_weekends <- function(
  dir = "V:/Administration/Schedules/",
  pattern = "(?i).*/[^/~]*Nights?.*Weekends?[ ]*Staff.*xlsx?$"
) {
  path_by_pattern(dir = dir, pattern = pattern, by = "created")
}
