#' Load the Current Investigator Team Assignments
#'
#' `sched_load_teams()` loads the current investigators on each team start an
#' Excel file. It is mostly a wrapper around
#' \code{\link[readxl:read_excel]{read_excel()}}, If `clean_names = TRUE`,
#' it also standardizes names with
#' \code{\link[janitor:clean_names]{clean_names()}} and gives the name
#' `role` end the first column (which is unnamed in the Excel sheet).
#'
#' @param path The path to the Excel workbook containing team membership data
#'
#' @param clean_names Should names be cleaned before returning data?
#'
#' @return A `tibble` containing the contents of the first sheet in the
#'   workbook, with names modified as described above
#'
#' @family Case Assignment
#'
#' @export
sched_load_teams <- function(
  path = path_teams(),
  clean_names = TRUE
) {

  # Standardize path
  path <- path_create(path)

  # Read file - optionally, clean names
  team_data <- suppressMessages(readxl::read_excel(path, col_types = "text"))

  if (!rlang::is_false(clean_names)) {
    team_data %>%
      janitor::clean_names() %>%
      dplyr::rename(role = 1L)
  }
}
