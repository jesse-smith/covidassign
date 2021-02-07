download_redcap_ids <- function(api_token = Sys.getenv("redcap_NCA_token")) {
  # URL base for API
  api_uri <- "https://redcap.shelbycountytn.gov/api/"

  api_params <- list(
    token = api_token,
    content='record',
    format='json',
    'fields[0]'='record_id'
  )

  httr::RETRY(
    "POST",
    api_uri,
    body = api_params,
    encode = "form",
    times = 12L,
    pause_cap = 300L
  ) %>%
    httr::stop_for_status(task = paste("download ids:", httr::content(.))) %>%
    to_tbl_redcap()
}
