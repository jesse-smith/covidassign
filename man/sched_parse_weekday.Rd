% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sched_parse_weekday.R
\name{sched_parse_weekday}
\alias{sched_parse_weekday}
\title{Parse Weekdays to Match Full Weekday Names}
\usage{
sched_parse_weekday(day)
}
\arguments{
\item{day}{A character vector of day names or abbreviations}
}
\value{
A character vector of full day names
}
\description{
\code{sched_parse_weekday()} takes a vector of weekday abbreviations and returns
the full name of each weekday. If the abbreviation is one letter, it matches
the standard one-letter weekday abbreviations (U/M/T/W/R/F/S). If more than
one letter, it matches the first two letters to the full day name.
}
\keyword{internal}
