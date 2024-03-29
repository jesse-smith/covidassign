% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign_acns.R
\name{assign_acns}
\alias{assign_acns}
\title{Assign Cases to Scheduled Investigators}
\usage{
assign_acns(
  .data = translate_acns(),
  date = attr(.data, "date"),
  api_token = Sys.getenv("redcap_NCA_token")
)
}
\arguments{
\item{.data}{Data frame of cases to assign, passed from
\code{\link[covidassign:translate_acns]{translate_acns()}}}

\item{date}{The date to use when checking whether investigators are scheduled}

\item{api_token}{The API token for the Case Assignment REDcap project. The
default pulls this from the environment variable \code{redcap_CA_token}.}
}
\value{
A \code{tibble} containing the input data with columns \code{investigator},
\code{team}, and \code{assign_date} added
}
\description{
\code{assign_acns()} divides unassigned cases among REDcap investigators
scheduled to work on \code{date}. It distributes cases randomly and evenly across
investigators until either all are assigned or there are too few cases for
another round of even distribution. Any remaining cases are assigned randomly
with at most one additional case given to an investigator.
}
\section{TODO}{

\enumerate{
\item Pull school age investigators from team lead sheet
\item Re-factor expressions to be self-documenting (helpers)
\item Add documentation to helpers
\item Create unit tests for all functions in file
}
}

