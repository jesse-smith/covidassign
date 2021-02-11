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

  httr::POST(
    # "POST",
    api_uri,
    body = api_params,
    encode = "form"
    # times = 12L,
    # pause_cap = 300L
  ) %>%
    httr::stop_for_status(task = paste("upload data:", httr::content(.))) %>%
    httr::content()
    # verify_and_archive_response(.data)
}

verify_and_archive_response <- function(content, reference) {

  n_content <- vec_size(content)
  n_reference <- vec_size(reference)
  response_ok <- n_content == n_reference

  if (!response_ok) {
    uploaded <- reference[["record_id"]] %in% purrr::flatten_chr(content)
    rejected_data <- dplyr::filter(reference, !uploaded)
    show(rejected_data)
    path_r <- coviData::path_create(
      "V:/EPI DATA ANALYTICS TEAM/Case Assignment/archive/rejected/",
      paste0("rejected_assigned_", lubridate::today()),
      ext = "csv"
    )
    path_a <- coviData::path_create(
      "V:/EPI DATA ANALYTICS TEAM/Case Assignment/archive/accepted/",
      paste0("accepted_assigned_", lubridate::today()),
      ext = "csv"
    )
    vroom::vroom_write(
      rejected_data,
      path = path_r,
      delim = ",",
      na = ""
    )
    vroom::vroom_write(
      dplyr::filter(reference, uploaded),
      path = path_a,
      delim = ",",
      na = ""
    )

    rlang::abort(
      paste(
        sum(!uploaded), "record(s) were rejected by REDcap;",
        "see the output for details"
      ),
      data = rejected_data
    )
  } else {
    path <- coviData::path_create(
      "V:/EPI DATA ANALYTICS TEAM/Case Assignment/archive/accepted/",
      paste0("assigned_", lubridate::today()),
      ext = "csv"
    )
    vroom::vroom_write(
      .data,
      path = path,
      delim = ",",
      na = ""
    )
    .data
  }
}
