download_redcap_metadata <- function(api_token = Sys.getenv("redcap_NCA_token")) {

  api_uri <- "https://redcap.shelbycountytn.gov/api/"

  api_params <- list(
    token = api_token,
    content = "metadata",
    format = "json"
  )

  httr::RETRY(
    "POST",
    api_uri,
    body = api_params,
    encode = "form",
    times = 12L,
    pause_cap = 300L
  ) %>%
    httr::stop_for_status() %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON() %>%
    dplyr::as_tibble()
}
