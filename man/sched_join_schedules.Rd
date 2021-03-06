% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sched_join_schedules.R
\name{sched_join_schedules}
\alias{sched_join_schedules}
\title{Join the \code{team_schedules} Dataset to Teams}
\usage{
sched_join_schedules(teams, schedules = covidassign::team_schedules)
}
\arguments{
\item{teams}{A data frame containing teams in columns and members in rows;
designed to take the output of
\code{\link[covidassign:sched_parse_teams]{sched_parse_teams()}}}

\item{schedules}{Schedules for each team, given by the \code{team_schedules}
dataset}
}
\value{
A \code{tibble} with columns for \code{team}, \code{member}, \code{schedule}, and
\code{anchor}
}
\description{
\code{sched_join_schedules()} pivots the \code{teams} data to long format and joins the
schedules specified in \code{\link{team_schedules}} to the result.
}
\seealso{
Other Case Assignment: 
\code{\link{sched_add_nights_weekends}()},
\code{\link{sched_calc}()},
\code{\link{sched_load_nights_weekends}()},
\code{\link{sched_load_teams}()},
\code{\link{sched_parse_nights_weekends}()},
\code{\link{sched_std_names}()},
\code{\link{schedule_predefined}()},
\code{\link{schedule}()}
}
\concept{Case Assignment}
