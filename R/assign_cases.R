#' Assign Cases to Scheduled Investigators
#'
#' `assign_cases()` divides unassigned cases among REDcap investigators
#' scheduled to work on `date`. It distributes cases randomly and evenly across
#' investigators until either all are assigned or there are too few cases for
#' another round of even distribution. Any remaining cases are assigned randomly
#' with at most one additional case given to an investigator.
#'
#' @inheritParams join_investigators
#'
#' @return A `tibble` containing the results of
#'   \code{
#'   \link[covidassign:download_redcap_cases]{download_redcap_cases()}
#'   }, with
#'   `assign_date` and `investigator` filled
#'
#' @export
assign_cases <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_CA_token")
) {

  cases <- download_redcap_cases(api_token = api_token)
  investigators <- join_investigators(date = date, api_token = api_token)

  # Get number of unassigned cases and investigators
  n_cases <- vctrs::vec_size(cases)
  n_investigators <- vctrs::vec_size(investigators)

  # Get number of investigators needed
  n_reps <- n_cases %/% n_investigators
  n_additional <- n_cases - n_investigators * n_reps

  # Create randomized vector of investigator assignments
  investigator_ids <- investigators %>%
    vctrs::vec_rep(times = n_reps) %>%
    dplyr::bind_rows(dplyr::slice_sample(investigators, n = n_additional)) %>%
    dplyr::slice_sample(prop = 1L) %>%
    dplyr::pull("id")

  # Replace `investigator` column with randomized `investigators` and fill
  # `assign_date`
  dplyr::mutate(
    cases,
    investigator = investigator_ids,
    assign_date = format(lubridate::now(), "%Y-%m-%d %H:%M")
  )
}

assign_school_age <- function(.data) {
  # Create age variable from dob, assign to Yvonne
}

assign_long_term_care <- function(.data) {
  # Parse addresses and assign those matching an LTCF to --?
}
