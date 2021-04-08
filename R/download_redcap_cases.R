#' Download Cases from Case Assignment REDcap Project
#'
#' `download_nit_records()` and `download_nca_records()` download records from
#' the \strong{Interview Tool} and \strong{Case Assignment} REDcap projects.
#'
#' @section TODO:
#' \enumerate{
#'   \item Check `six` column in NCA to see if exclusion is necessary
#'   \item Create unit tests
#' }
#'
#' @param api_token The API token for the Case Assignment REDcap project
#'
#' @param fields `character`. The fields to download from the project.
#'   By default, all fields are downloaded.
#'
#' @return A `tibble` with all variables in `character` format
#'
#' @name download_redcap_records
NULL

#' @rdname download_redcap_records
download_nca_records <- function(
  api_token = Sys.getenv("redcap_NCA_token"),
  fields = NULL
) {

  # URL base for API
  api_uri <- "https://redcap.shelbycountytn.gov/api/"

  api_params <- list(
    token = api_token,
    content = "record",
    format = "json",
    type = "flat"
  ) %>% append(redcap_fields(fields))

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

#' @rdname download_redcap_records
download_nit_records <- function(
  api_token = Sys.getenv("redcap_NIT_token"),
  fields = NULL
) {
  # URL base for API
  api_uri <- "https://redcap.shelbycountytn.gov/api/"

  api_params <- list(
    token = api_token,
    content = "record",
    format = "json",
    type = "flat"
  ) %>% append(redcap_fields(fields))

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
    dplyr::as_tibble()
}

#' Select REDcap Fields to Download
#'
#' `redcap_fields()` converts a `character` vector to a named `list` that can
#' be appended to the REDcap API parameters
#'
#' @param fields `character`. REDcap fields to select.
#'
#' @return A named `list`
redcap_fields <- function(fields) {
  if (vec_is_empty(fields)) return(NULL)
  fields %>%
    set_names(paste0("fields[", seq_along(.) - 1L, "]")) %>%
    as.list()
}
