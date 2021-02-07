#' Download Cases from Case Assignment REDcap Project
#'
#' `download_redcap_cases()` downloads records from the
#' \strong{Case Assignment} REDcap project. If `incl_assigned = FALSE`, it only
#' downloads unassigned cases; if `incl_assigned = TRUE`, it downloads all
#' cases.
#'
#' @param api_token The API token for the Case Assignment REDcap project. The
#' default pulls this from the environment variable `redcap_CA_token`
#'
#' @param unassigned_only Should only unassigned cases be downloaded?
#'
#' @return A tidy `tibble` with all variables in `character` format
download_redcap_cases <- function(
  api_token = Sys.getenv("redcap_NCA_token")
) {

  # URL base for API
  api_uri <- "https://redcap.shelbycountytn.gov/api/"

  api_params <- list(
    token = api_token,
    content = "record",
    format = "json",
    type = "flat"
  )

  httr::RETRY(
    "POST",
    api_uri,
    body = api_params,
    times = 12L,
    pause_cap = 300L
  ) %>%
    httr::stop_for_status() %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON() %>%
    dplyr::as_tibble() %>%
    dplyr::select(-dplyr::matches("^six$"))
}
