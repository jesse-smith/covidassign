#' Validate Case Assignments
#'
#' `validate_assignments()` validates that cases were assigned to all scheduled
#' investigators. By default, it archives any excluded investigators and
#' notifies relevant parties of the omission.
#'
#' @param .data Assignment data from `assign_acns()`
#'
#' @param archive Should excluded investigators be archived?
#'
#' @param notify_to Email addresses to notify of excluded investigators. Set to
#'   `NULL` to disable notifications.
#'
#' @param cnd Should excluded investigators trigger a warning or an error?
#'
#' @param date The date assigned to the output `date_tbl`
#'
#' @return Excluded investigators in a `date_tbl`
#'
#' @export
validate_assignments <- function(
  .data = assign_acns(),
  archive = TRUE,
  notify_to = c(
    "Jesse.Smith@shelbycountytn.gov",
    "Karim.Gilani@shelbycountytn.gov"
  ),
  cnd = c("warning", "error"),
  date = attr(.data, "date")
) {

  cnd <- rlang::arg_match(cnd)[[1L]]

  sched_invs <- dplyr::left_join(
    suppressMessages(sched_investigators(date = date)),
    get_investigators(date = date),
    by = c("team", "investigator")
  )

  # Get scheduled investigators who did not receive cases
  excl_inv <- dplyr::anti_join(
    sched_invs,
    dplyr::transmute(.data, id = as.character(.data[["investigator"]])),
    by = "id"
  ) %>%
    dplyr::mutate(id = as.integer(.data[["id"]])) %>%
    dplyr::rename(redcap_id = .data[["id"]]) %>%
    covidsms::as_date_tbl(date = date)

  any_excl <- !vec_is_empty(excl_inv)

  archive_path <- if (archive) archive_excl_inv(excl_inv)

  notify <- !vec_is_empty(notify_to)
  if (notify && any_excl) {
    notify_excl_inv(excl_inv, to = notify_to, path = archive_path, date = date)
  }

  if (any_excl) {
    msg <- paste0(
      vec_size(excl_inv), " investigators were not assigned cases:",
      "\n",
      paste0(utils::capture.output(print(excl_inv, n = Inf)), collapse = "\n")
    )

    if (cnd == "warning") rlang::warn(msg) else rlang::abort(msg)
  }

  excl_inv
}

archive_excl_inv <- function(
  excl_inv,
  dir = path_create("V:/EPI DATA ANALYTICS TEAM/Case Assignment/",
                    "data/excluded_investigators/"),
  force = FALSE
) {
  is_empty <- vec_is_empty(excl_inv)
  path <- path_create(
    dir,
    paste0("excl_inv_", lubridate::today(), if (is_empty) "_EMPTY"),
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
    coviData::write_file_delim(excl_inv, path = path)
  }

  path
}

notify_excl_inv <- function(
  excl_inv,
  to,
  path = NULL,
  date = attr(excl_inv, "date")
) {

  excl_inv <- as.data.frame(excl_inv)

  if (rlang::is_installed("gt")) {
    tbl <- excl_inv %>% gt::gt() %>% gt::as_raw_html()
  } else {
    tbl <- paste0(utils::capture.output(print(excl_inv)), collapse = "<br>")
  }

  archive <- is.null(path)

  dt_fmt <- format(date, "%m/%d/%y")

  subject <- paste0(
    "Investigators Excluded from Assignment (", ")"
  )

  body <- stringr::str_glue(
    "The following investigators are scheduled, but were not assigned cases:",
    "<br>",
    tbl,
    if (archive) "<br><br>" else "",
    if (archive) "See <a href='file:///{path}'>{path}</a> " else "",
    if (archive) "for the archived spreadsheet." else ""
  )

  coviData::notify(to = to, subject = subject, body = body, html = TRUE)
}
