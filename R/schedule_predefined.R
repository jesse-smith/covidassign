#' Quickly Create Pre-Defined Work Schedules
#'
#' @description
#'
#' These functions create work schedules for weekday, 4-2, and
#' (5-2)-(5-3)-(6-2)x4-(6-3) schedules between the given `start` and `end`
#' dates. Rotating schedules (the latter two) require specification of a
#' starting ("anchor") point for the scheduling calculation; this is supplied
#' using the `anchor` argument, which is set end `start` by default. Weekly
#' schedules do not require an anchor point.
#'
#' `schedule_weekdays()` defines a Monday-Friday work schedule.
#'
#' `schedule_42()` defines a rotating 4 on, 2 off work schedule.
#'
#' `schedule_5623()` defines a rotating schedule with the following pattern:
#' \itemize{
#'   \item{5 on, 2 off}
#'   \item{5 on, 3 off}
#'   \item{6 on, 2 off}
#'   \item{6 on, 2 off}
#'   \item{6 on, 2 off}
#'   \item{6 on, 2 off}
#'   \item{6 on, 3 off}
#' }
#'
#' `schedule_526264()` defines a rotating schedule with the following pattern:
#' \itemize{
#'   \item{5 on, 2 off}
#'   \item{5 on, 2 off}
#'   \item{5 on, 2 off}
#'   \item{6 on, 2 off}
#'   \item{(repeat 4 more times...)}
#'   \item{5 on, 2 off}
#'   \item{5 on, 2 off}
#'   \item{5 on, 2 off}
#'   \item{5 on, 4 off}
#' }
#'
#' @details
#'
#' These functions depend on the more general
#' \code{\link[covidassign:schedule]{schedule()}} function, which takes
#' an arbitrary weekly or rotating schedule and (if needed) an anchor point. See
#' that function for implementing other schedules.
#'
#' @inherit schedule params return
#'
#' @param schedule A string indicating the schedule to use. For custom
#'   schedules, use `schedule = "custom"`.
#'
#' @family Case Assignment
#'
#' @aliases schedule_weekdays schedule_42 schedule_5623
schedule_predefined <- function(
  schedule = c("weekdays", "42", "5623", "526254", "custom"),
  start = Sys.Date(),
  end = start + 29L,
  anchor = start,
  cycle = NULL
) {

  schedule <- rlang::arg_match(schedule)[[1L]]

  if (schedule == "weekdays") {
    schedule_weekdays(start = start, end = end, anchor = anchor)
  } else if (schedule == "42") {
    schedule_42(start = start, end = end, anchor = anchor)
  } else if (schedule == "5623") {
    schedule_5623(start = start, end = end, anchor = anchor)
  } else {
    schedule(cycle = cycle, start = start, end = end, anchor = anchor)
  }
}

#' @rdname schedule_predefined
#'
#' @export
schedule_weekdays <- function(
  start = Sys.Date(),
  end = start + 29L,
  anchor = start
) {

  # Define weekday cycle
  cycle <- c(
    Sunday = FALSE,
    Monday = TRUE,
    Tuesday = TRUE,
    Wednesday = TRUE,
    Thursday = TRUE,
    Friday = TRUE,
    Saturday = FALSE
  )

  schedule(
    cycle = cycle,
    start = start,
    end = end
  )
}

#' @rdname schedule_predefined
#'
#' @export
schedule_42 <- function(
  start = Sys.Date(),
  end = Sys.Date() + 29L,
  anchor = start
) {

  cycle <- c(rep(TRUE, 4L), rep(FALSE, 2L))

  schedule(
    cycle = cycle,
    start = start,
    end = end,
    anchor = anchor
  )
}

#' @rdname schedule_predefined
#'
#' @export
schedule_5623 <- function(
  start = Sys.Date(),
  end = Sys.Date() + 29L,
  anchor = start
) {

  c52 <- c(rep(TRUE, 5L), rep(FALSE, 2L))
  c53 <- c(rep(TRUE, 5L), rep(FALSE, 3L))
  c62 <- c(rep(TRUE, 6L), rep(FALSE, 2L))
  c63 <- c(rep(TRUE, 6L), rep(FALSE, 3L))

  cycle <- c(c52, c53, rep(c62, 4L), c63)

  schedule(
    cycle = cycle,
    start = start,
    end = end,
    anchor = anchor
  )
}

#' @rdname schedule_predefined
#'
#' @export
schedule_526264 <- function(
  start = Sys.Date(),
  end = Sys.Date() + 29L,
  anchor = start
) {
  c52 <- c(rep(TRUE, 5L), rep(FALSE, 2L))
  c62 <- c(rep(TRUE, 6L), rep(FALSE, 2L))
  c54 <- c(rep(TRUE, 6L), rep(FALSE, 4L))

  c5262 <- c(rep(c52, 3L), c62)

  cycle <- c(rep(c5262, 5L), c54)

  schedule(
    cycle = cycle,
    start = start,
    end = end,
    anchor = anchor
  )
}
