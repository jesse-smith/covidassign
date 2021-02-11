#' Prepare Assigned Cases for REDcap Upload
#'
#' `prep_acns_redcap()` performs final data cleaning to prepare assigned cases
#' for upload to REDcap.
#'
#' @param .data Assigned case data
#'
#' @return Data ready for upload via `upload_assignments()`
#'
#' @export
prep_acns_redcap <- function(.data = assign_acns()) {

  .data %>%
    # Parse existing columns
    dplyr::mutate(
      report_d = format(.data[["report_d"]], "%Y-%m-%d %H:%M"),
      assign_date = format(.data[["assign_date"]], "%Y-%m-%d %H:%M"),
      dob = format(.data[["dob"]], "%Y-%m-%d"),
      specimen_date = format(.data[["specimen_date"]], "%Y-%m-%d"),
      investigator = as.character(.data[["investigator"]]),
      team = as.character(.data[["team"]]),
      first_name = stringi::stri_trans_totitle(
        .data[["first_name"]],
        type = "word"
      ),
      last_name = stringi::stri_trans_totitle(
        .data[["last_name"]],
        type = "word"
      ),
      address = stringi::stri_trans_totitle(
        .data[["address"]],
        type = "word"
      )
    ) %>%
    # Add `record_id`
    create_acns_record_id() %>%
    # Replace missing with empty string
    dplyr::mutate(dplyr::across(.fns = ~ stringr::str_replace_na(.x, ""))) %>%
    # Relocate REDcap columns to front, in correct order
    relocate_acns_redcap() %>%
    distinct_assigned()
}

create_acns_record_id <- function(.data, force = FALSE) {

  if ("record_id" %in% colnames(.data) && !force) return(.data)

  redcap_ids <- download_redcap_ids()

  if (vec_is_empty(redcap_ids)) {
    max_id <- 0L
  }
  max_id <- download_redcap_ids() %>%
    purrr::when(vec_is_empty(.) ~ dplyr::tibble(record_id = 0L), ~ .) %>%
    dplyr::pull(1L) %>%
    as.integer() %>%
    max(na.rm = TRUE) %>%
    purrr::when(abs(.) == Inf ~ 0L, ~ .)

  dplyr::mutate(.data, record_id = as.character(dplyr::row_number() + max_id))
}

relocate_acns_redcap <- function(.data) {

  template <- download_redcap_template()

  cols <- dplyr::intersect(template[["field_name"]], colnames(.data))

  dplyr::relocate(.data, {{ cols }}, .before = 1L)
}

distinct_assigned <- function(.data) {

  assigned <- download_redcap_cases() %>%
    dplyr::mutate(phone = covidsms::std_phone(.data[["phone"]]))

  tidylog::anti_join(
    .data,
    assigned,
    by = c("first_name", "last_name", "dob", "phone")
  )
}
