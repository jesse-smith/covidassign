#' Validate Case Assignment Upload
#'
#' `validate_upload()` validates that all assigned cases were uploaded. By
#' default, it archives accepted and rejected cases and notifies relevant
#' parties of the status. Note that, unlike `validate_assignments()`, a
#' notification is sent even if all cases are uploaded correctly.
#'
#' @param .data Upload data from `upload_assigments()`
#'
#' @param archive Should accepted and rejected cases be archived?
#'
#' @param notify_to Email addresses to notify of case assignment. Set to `NULL`
#'   to disable notification.
#'
#' @param cnd Should rejected cases trigger a warning or an error?
#'
#' @param date The date to use in notification
#'
#' @return The input data
#'
#' @export
validate_upload <- function(
  .data,
  archive = TRUE,
  notify_to = c(
    "Jesse.Smith@shelbycountytn.gov",
    "Karim.Gilani@shelbycountytn.gov"
  ),
  cnd = c("warning", "error"),
  date = attr(.data, "date")
) {

  cnd <- rlang::arg_match(cnd)[[1L]]

  if (archive) {
    accepted_path <- archive_accepted(.data)
    rejected_path <- archive_rejected(.data)
  }

  notify <- !vec_is_empty(notify_to)
  if (notify) {
    notify_upload(
      .data,
      to = notify_to,
      path = if (archive) fs::path_common(c(accepted_path, rejected_path)),
      date = date
    )
  }

  rejected <- dplyr::filter(.data, !.data[["uploaded"]])

  if (!vec_is_empty(rejected)) {
    rejected_msg <- paste0(
      utils::capture.output(print(rejected), n = Inf),
      collapse = "\n"
    )
    msg <- paste0(
      sum(.data[["uploaded"]], na.rm = TRUE), " records were not uploaded ",
      "to the assignment project:",
      "\n\n",
      rejected_msg
    )

    if (cnd == "warning") rlang::warn(msg) else rlang::abort(msg)
  }

  .data
}

archive_accepted <- function(
  .data,
  dir = "V:/EPI DATA ANALYTICS TEAM/Case Assignment/data/archive/accepted/",
  force = FALSE
) {
  accepted <- dplyr::filter(.data, .data[["uploaded"]])
  is_empty <- vec_is_empty(accepted)
  path <- coviData::path_create(
    dir,
    paste0("accepted_assigned_", lubridate::today(), if (is_empty) "_EMPTY"),
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
    coviData::write_file_delim(accepted, path = path)
  }

  path
}

archive_rejected <- function(
  .data,
  dir = "V:/EPI DATA ANALYTICS TEAM/Case Assignment/data/archive/rejected/",
  force = FALSE
) {
  rejected <- dplyr::filter(.data, !.data[["uploaded"]])
  is_empty <- vec_is_empty(rejected)
  path <- coviData::path_create(
    dir,
    paste0("rejected_assigned_", lubridate::today(), if (is_empty) "_EMPTY"),
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
    coviData::write_file_delim(rejected, path = path)
  }

  path
}

notify_upload <- function(
  .data,
  to,
  path = NULL,
  date = attr(.data, "date")
) {

  tbl <- .data %>%
    janitor::tabyl("uploaded") %>%
    janitor::adorn_totals() %>%
    janitor::adorn_pct_formatting()

  if (rlang::is_installed("gt")) {
    tbl <- tbl %>% gt::gt() %>% gt::as_raw_html()
  } else {
    tbl <- paste0(utils::capture.output(print(tbl)), collapse = "<br>")
  }

  dt_fmt <- format(date, "%m/%d/%y")

  subject <- paste0("Case Assignments Upload (", dt_fmt, ")")

  body <- stringr::str_glue(
    "Case assignments for ", dt_fmt, " have been uploaded to REDcap as below:",
    "<br><br>",
    tbl,
    if (!is.null(path)) "<br><br>" else "",
    if (!is.null(path)) "Please see <a href='{path}'>{path}</a>" else "",
    if (!is.null(path)) "for successful and failed uploads." else ""
  )

  coviData::notify(to = to, subject = subject, body = body, html = TRUE)
}
