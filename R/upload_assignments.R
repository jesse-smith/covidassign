#' Upload Assigned Cases to REDcap Case Assignment Project
#'
#' `upload_assignments()` uploads assigned cases to the Case Assignment REDcap
#' project, with `Investigator` and `Date of Assignment` fields filled.
#'
#' @param .data Assigned cases data
#'
#' @param api_token API token for REDcap project
#'
#' @param size_limit Size limit of REDcap upload, in bytes
#'
#' @return An httr `Response` w/ content indicating the number of uploaded
#'   records
#'
#' @export
upload_assignments <- function(
  .data,
  api_token = Sys.getenv("redcap_NCA_token"),
  size_limit = as.integer(0.9 * 32e6)
) {

  # URL base for API
  api_uri <- "https://redcap.shelbycountytn.gov/api/"

  api_params <- list(
    token = api_token,
    content = "record",
    format = "json",
    type = "flat",
    data = to_json_redcap(.data),
    returnContent = "ids"
  )

  httr::RETRY(
    "POST",
    api_uri,
    body = api_params,
    encode = "form",
    times = 12L,
    pause_cap = 300L
  ) %>%
    httr::stop_for_status(task = paste("upload data:", httr::content(.))) %>%
    convert_assignment_response(data = .data)
}

convert_assignment_response <- function(response, data) {
  ids <- response %>% httr::content() %>% purrr::flatten_chr()
  dplyr::mutate(
    data,
    uploaded = tidyr::replace_na(.data[["record_id"]] %in% ids, FALSE)
  )
}
