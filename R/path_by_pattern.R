#' @rdname path_by_pattern
#'
#' @export
path_teams <- function(
  dir = "V:/Administration/Schedules/",
  pattern = "(?i).*/[^/~]*Investigations?[ ]*Staff[ ]*Schedule.*xlsx?$"
) {
  path_by_pattern(dir = dir, pattern = pattern)
}

#' @rdname path_by_pattern
#'
#' @export
path_nights_weekends <- function(
  dir = "V:/Administration/Schedules/",
  pattern = "(?i).*/[^/~]*Nights?.*Weekends?[ ]*Staff.*xlsx?$"
) {
  path_by_pattern(dir = dir, pattern = pattern)
}

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
#' These functions are powered (and generalized) by `path_by_pattern()`, which
#' finds the most recent file in a given directory that matches a regular
#' expression.
#'
#' @param dir The path to the directory of interest
#'
#' @param pattern The regular expression to use when filtering paths
#'
#' @return An `fs_path` vector
#'
#' @aliases path_teams path_nights_weekends
#'
#' @export
path_by_pattern <- function(dir, pattern) {
  # Create/clean `dir` path
  dir <- path_create(dir)

  # Get list of files in `dir` matching `pattern`
  fs::dir_info(
    path = dir,
    regexp = pattern,
    type = "file"
  ) %>%
    dplyr::filter(
      .data[["birth_time"]] == max(.data[["birth_time"]], na.rm = TRUE)
    ) %>%
    dplyr::pull(.data[["path"]])
}
