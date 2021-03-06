% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schedule_predefined.R
\name{schedule_predefined}
\alias{schedule_predefined}
\alias{schedule_weekdays}
\alias{schedule_42}
\alias{schedule_5623}
\alias{schedule_526264}
\title{Quickly Create Pre-Defined Work Schedules}
\usage{
schedule_predefined(
  schedule = c("weekdays", "42", "5623", "526254", "custom"),
  start = Sys.Date(),
  end = start + 29L,
  anchor = start,
  cycle = NULL
)

schedule_weekdays(start = Sys.Date(), end = start + 29L, anchor = start)

schedule_42(start = Sys.Date(), end = Sys.Date() + 29L, anchor = start)

schedule_5623(start = Sys.Date(), end = Sys.Date() + 29L, anchor = start)

schedule_526264(start = Sys.Date(), end = Sys.Date() + 29L, anchor = start)
}
\arguments{
\item{schedule}{A string indicating the schedule to use. For custom
schedules, use \code{schedule = "custom"}.}

\item{start}{The start date of the returned schedule; either a string in
"YYYY-MM-DD" format or a \code{Date} object}

\item{end}{The end date of the returned schedule; either a string in
"YYYY-MM-DD" format or a \code{Date} object}

\item{anchor}{The date from which to start ("anchor") schedule calculations.
This can be any valid date; no particular relationship to \code{start} or \code{end}
is needed. It must be either a string in "YYYY-MM-DD" format or a
\code{Date} object.}

\item{cycle}{A logical vector defining one scheduling cycle. If named, names
are passed to
\code{\link[covidassign:sched_parse_weekday]{sched_parse_weekday()}} for
standardization of weekday names.}
}
\value{
A \code{tibble} with columns \code{date} (a \code{Date} column containing dates
between \code{start} and \code{end}, inclusive), \code{weekday} (a \code{character} column
containing full weekday names), and \code{scheduled} (a \code{logical} column
defining whether a day is schedule ("on") or not ("off"))
}
\description{
These functions create work schedules for weekday, 4-2, and
(5-2)-(5-3)-(6-2)x4-(6-3) schedules between the given \code{start} and \code{end}
dates. Rotating schedules (the latter two) require specification of a
starting ("anchor") point for the scheduling calculation; this is supplied
using the \code{anchor} argument, which is set end \code{start} by default. Weekly
schedules do not require an anchor point.

\code{schedule_weekdays()} defines a Monday-Friday work schedule.

\code{schedule_42()} defines a rotating 4 on, 2 off work schedule.

\code{schedule_5623()} defines a rotating schedule with the following pattern:
\itemize{
\item{5 on, 2 off}
\item{5 on, 3 off}
\item{6 on, 2 off}
\item{6 on, 2 off}
\item{6 on, 2 off}
\item{6 on, 2 off}
\item{6 on, 3 off}
}

\code{schedule_526264()} defines a rotating schedule with the following pattern:
\itemize{
\item{5 on, 2 off}
\item{5 on, 2 off}
\item{5 on, 2 off}
\item{6 on, 2 off}
\item{(repeat 4 more times...)}
\item{5 on, 2 off}
\item{5 on, 2 off}
\item{5 on, 2 off}
\item{5 on, 4 off}
}
}
\details{
These functions depend on the more general
\code{\link[covidassign:schedule]{schedule()}} function, which takes
an arbitrary weekly or rotating schedule and (if needed) an anchor point. See
that function for implementing other schedules.
}
\seealso{
Other Case Assignment: 
\code{\link{sched_add_nights_weekends}()},
\code{\link{sched_calc}()},
\code{\link{sched_join_schedules}()},
\code{\link{sched_load_nights_weekends}()},
\code{\link{sched_load_teams}()},
\code{\link{sched_parse_nights_weekends}()},
\code{\link{sched_std_names}()},
\code{\link{schedule}()}
}
\concept{Case Assignment}
