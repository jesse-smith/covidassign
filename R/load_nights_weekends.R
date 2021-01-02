#' Load Night & Weekend Scheduling Data
#'
#' `asg_load_nights_weekends()` loads schedules for investigators working nights
#' and weekends. These team members do not follow a team-wide schedule and must
#' be dealt with individually.
#'
#' @param path The location of the nights and weekends scheduling Excel workbook
#'
#' @param sheet The sheet from which to read the schedule; either the sheet name
#'   or the sheet number
#'
#' @param clean_names Should names be cleaned?
#'
#' @return The data in the read sheet as a `tibble`
#'
#' @family Case Assignment
#'
#' @export
asg_load_nights_weekends <- function(
  path = path_create(
    "V:/Administration/Schedules/Night&Weekend staff",
    ext = "xlsx"
  ),
  sheet = "schedules",
  clean_names = TRUE
) {

  path <- path_create(path)

  data <- readxl::read_excel(path, sheet = sheet, col_types = "text")

  if (!rlang::is_false(clean_names)) {
    janitor::clean_names(data)
  }
}
