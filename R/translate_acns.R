#' Translate ACNS Data to REDcap-Useable Format
#'
#' `translate_acns()` maps fields from ACNS data to REDcap data and excludes
#' duplicates + cases older than `days` old.
#'
#' @param .data ACNS data. Runs `covidsms::prep_acns(assign = TRUE)` by default.
#'
#' @param date The date to use for assignment; unlike in other functions, this
#'   defaults to today's date as part of the "translation" process.
#'
#' @param days Number of days back to consider valid. `test_date` older than
#'   this will not be assigned.
#'
#' @param archive Should data be archived (before deduplicating)?
#'
#' @return The data mapped to REDcap field names
#'
#' @export
translate_acns <- function(
  .data = covidsms::prep_acns(assign = TRUE),
  date = lubridate::today(),
  days = 6L,
  archive = TRUE
) {
  translated <- .data %>%
    dplyr::select(
      local_id = "pkey",
      report_d = "date_added",
      "nbs",
      "first_name",
      "last_name",
      dob = "date_of_birth",
      specimen_date = "test_date",
      phone = "pnumber",
      dplyr::everything()
    ) %>%
    dplyr::mutate(
      address = paste(
        stringr::str_replace_na(.data[["addr1"]], ""),
        stringr::str_replace_na(.data[["addr2"]], ""),
        stringr::str_replace_na(.data[["city"]], ""),
        stringr::str_replace_na(.data[["state"]], ""),
        stringr::str_replace_na(.data[["zip"]], "")
      ) %>% covidsms::std_addr(),
      .before = "school_age"
    ) %>%
    janitor::clean_names()

  if (archive) archive_translated(translated)

  translated %>%
    dplyr::select(
      -c("result", "sex", "addr1", "addr2", "city", "state", "zip")
    ) %>%
    dplyr::filter(
      !.data[["duplicate"]],
      (.data[["specimen_date"]] >= date - days) | is.na(.data[["specimen_date"]])
    ) %>%
    dplyr::select(-"duplicate") %>%
    covidsms::as_date_tbl(date = date)
}

#' Archive Translated ACNS Data
#'
#' `archive_translated()` archives cases translated from the output of
#' \code{\link[covidsms:prep_acns]{prep_acns(assign = TRUE)}}. It saves the
#' translated data without deduplicating.
#'
#' @param data Data from `prep_acns(assign = TRUE)`
#'
#' @param dir Archive directory
#'
#' @param force Should an existing file be overwritten?
#'
#' @return The path to the archive file
archive_translated <- function(
  data,
  dir = "V:/EPI DATA ANALYTICS TEAM/Case Assignment/data/archive/new_tests",
  force = FALSE
) {
  is_empty <- vec_is_empty(data)
  path <- path_create(
    dir,
    paste0("new_tests_", lubridate::today(), if (is_empty) "_EMPTY"),
    ext = "csv"
  )

  if (!force && fs::file_exists(path)) {
    rlang::inform(
      paste(
        "A file already exists in this location; no new data will be written.",
        "To overwrite the existing file, set `force = TRUE`."
      )
    )
  } else {
    coviData::write_file_delim(data, path = path)
  }

  path
}
