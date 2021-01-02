#' Download Investigators Listed in the Case Assignment REDcap Project
#'
#' `asg_download_redcap_investigators()` downloads all investigators available
#' for assignment in the \strong{Case Assignment} REDcap project, as well as the
#' `id` assigned to each investigator in REDcap.
#'
#' @param api_token The API token for the Case Assignment REDcap project. The
#' default pulls this from the environment variable `redcap_CA_token`.
#'
#' @return A `tibble` with one row for each investigator and columns for `id`
#'   and `investigator` (name)
#'
#' @export
asg_download_redcap_investigators <- function(
  api_token = Sys.getenv("redcap_CA_token")
) {
  # URL base for API
  api_uri <- "https://redcap.health.tn.gov/redcap/api/"

  api_params <- list(
    token = api_token,
    format = "json",
    content = "metadata"
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
    dplyr::filter(.data[["field_name"]] == "investigator") %>%
    dplyr::pull("select_choices_or_calculations") %>%
    stringr::str_split(pattern = " [|] ") %>%
    purrr::flatten_chr() %>%
    tibble::as_tibble_col("inv") %>%
    tidyr::separate(
      col = "inv",
      into = c("id", "investigator"),
      sep = "[,] "
    ) %>%
    dplyr::mutate(
      investigator = asg_parse_names(.data[["investigator"]])
    )
}
