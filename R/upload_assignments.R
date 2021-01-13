#' Upload Assigned Cases to REDcap Case Assignment Project
#'
#' `upload_assignments()` uploads assigned cases to the Case Assignment REDcap
#' project, with `Investigator` and `Date of Assignment` fields filled.
#'
#' @param .data Assigned cases data
#'
#' @param api_token API token for REDcap project
#'
#' @return An httr `Response` w/ content indicating the number of uploaded
#'   records
#'
#' @export
upload_assignments <- function(
  .data,
  api_token = Sys.getenv("redcap_CA_token")
) {
  # URL base for API
  api_uri <- "https://redcap.health.tn.gov/redcap/api/"

  api_params <- list(
    token = api_token,
    content = "record",
    format = "json",
    type = "flat",
    data = jsonlite::toJSON(.data)
  )

  httr::RETRY(
    "POST",
    api_uri,
    body = api_params,
    times = 12L,
    pause_cap = 300L
  ) %>%
    httr::stop_for_status()
}
