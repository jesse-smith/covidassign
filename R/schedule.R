#' Define Rotating or Weekly Work Schedules
#'
#' `schedule()` defines work schedules based off of a given `cycle`. If this
#' cycle is named, it is assumed that the names are days of the week, and that
#' the schedule repeats weekly. If it is unnamed, the `cycle` begins at the
#' `anchor` date. Defining rotating weekly schedules
#' (i.e. T/R one week, M/W/F the next) is not currently supported using weekday
#' names; these schedules may be defined as a rotating schedule anchored to the
#' beginning of a week.
#'
#' `schedule_by_cycle()` and `schedule_by_day()` are the workhorses
#' underlying `as_schedule()`. They handle the general cyclic and weekly use
#' cases described above.
#'
#' @param cycle A logical vector defining one scheduling cycle. If named, names
#'   are passed to \code{\link[covidassign:parse_weekday]{parse_weekday()}} for
#'   standardization of weekday names.
#'
#' @param start The start date of the returned schedule; either a string in
#'   "YYYY-MM-DD" format or a `Date` object
#'
#' @param end The end date of the returned schedule; either a string in
#'   "YYYY-MM-DD" format or a `Date` object
#'
#' @param anchor The date from which to start ("anchor") schedule calculations.
#'   This can be any valid date; no particular relationship to `start` or `end`
#'   is needed. It must be either a string in "YYYY-MM-DD" format or a
#'   `Date` object.
#'
#' @return A `tibble` with columns `date` (a `Date` column containing dates
#'   between `start` and `end`, inclusive), `weekday` (a `character` column
#'   containing full weekday names), and `scheduled` (a `logical` column
#'   defining whether a day is schedule ("on") or not ("off"))
#'
#' @family Case Assignment
#'
#' @aliases schedule_by_cycle schedule_by_day
#'
#' @export
schedule <- function(
  cycle = c(
    Sun = FALSE,
    Mon = TRUE,
    Tue = TRUE,
    Wed = TRUE,
    Thu = TRUE,
    Fri = TRUE,
    Sat = FALSE
  ),
  start = Sys.Date(),
  end = Sys.Date() + 29L,
  anchor = start
) {

  cycle_is_named <- !is.null(names(cycle))

  start  <- start  %>% std_dates() %>% lubridate::as_date()
  end    <- end    %>% std_dates() %>% lubridate::as_date()
  anchor <- anchor %>% std_dates() %>% lubridate::as_date()

  if (cycle_is_named) {
    schedule_by_day(
      cycle = cycle,
      start = start,
      end = end
    )
  } else {
    schedule_by_cycle(
      cycle = cycle,
      start = start,
      end = end,
      anchor = anchor
    )
  }
}
