% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_redcap_cases.R
\name{download_redcap_cases}
\alias{download_redcap_cases}
\title{Download Cases from Case Assignment REDcap Project}
\usage{
download_redcap_cases(api_token = Sys.getenv("redcap_NCA_token"))
}
\arguments{
\item{api_token}{The API token for the Case Assignment REDcap project}
}
\value{
A \code{tibble} with all variables in \code{character} format
}
\description{
\code{download_redcap_cases()} downloads records from the
\strong{Case Assignment} REDcap project.
}
\section{TODO}{

\enumerate{
\item Check \code{six} column in NCA to see if exclusion is necessary
\item Create unit tests
}
}

