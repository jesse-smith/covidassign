#' Prepare Assigned Cases for REDcap Upload
#'
#' `prep_acns_redcap()` performs final data cleaning to prepare assigned cases
#' for upload to REDcap.
#'
#' @param .data Assigned case data
#'
#' @param date The date to assume for "today"; default is given by the `date`
#'   attribute of the input `date_tbl`
#'
#' @return Data ready for upload via `upload_assignments()`
#'
#' @export
prep_acns_redcap <- function(
  .data = assign_acns(),
  date = attr(.data, "date")
) {

  .data %>%
    # Parse existing columns
    dplyr::mutate(
      report_d = format(.data[["report_d"]], "%Y-%m-%d %H:%M"),
      assign_date = format(.data[["assign_date"]], "%Y-%m-%d %H:%M"),
      dob = format(.data[["dob"]], "%Y-%m-%d"),
      specimen_date = format(.data[["specimen_date"]], "%Y-%m-%d"),
      investigator = as.character(.data[["investigator"]]),
      team = as.character(.data[["team"]]),
      nbs = as.character(.data[["nbs"]]),
      first_name = coviData::std_names(.data[["first_name"]], case = "title"),
      last_name = coviData::std_names(.data[["last_name"]], case = "title"),
      address = stringr::str_to_title(.data[["address"]]),
      assigner = as.integer(5)
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

distinct_assigned <- function(.data, archive = TRUE) {

  # Columns to use when joining previous assignments with `.data` below
  join_cols <- c("first_name", "last_name", "dob")

  # Columns to download from REDcap - NCA
  nca_cols <- c(
    "record_id",
    "first_name",
    "last_name",
    "dob"
  )

  # Previous assignments are loaded/parsed for removal from `.data`
  assigned <- download_nca_records(fields = nca_cols) %>%
    dplyr::mutate(
      first_name = coviData::std_names(.data[["first_name"]], case = "title"),
      last_name = coviData::std_names(.data[["last_name"]], case = "title"),
      dob = .data[["dob"]] %>%
        stringr::str_replace("^$", replacement = NA_character_) %>%
        std_dates(
          orders = c("ymd", "ymdHM", "ymdHMS", ""),
          force = "dt",
          train = FALSE
        ) %>%
        format(format = "%Y-%m-%d")
    ) %>%
    dplyr::select("record_id", {{ join_cols }})

  # `.data` needs a `record_id` for joining
  data <- create_acns_record_id(.data, force = TRUE)
  remove(.data)

  if (rlang::is_installed("tidylog")) {
    data_distinct <- tidylog::anti_join(data, assigned, by = join_cols)
  } else {
    data_distinct <- dplyr::anti_join(data, assigned, by = join_cols)
  }

  dplyr::select(data_distinct, -dplyr::matches("^[.].*_tmp_$"))
}
