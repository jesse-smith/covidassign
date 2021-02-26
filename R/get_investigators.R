#' Join Investigators Listed in Investigation Schedule & Case Assignment Project
#'
#' `get_investigators()` joins the investigators scheduled to work on
#' `date` with the list of investigators from the \strong{Case Assignment}
#' REDcap project. By default, it performs an inner join, since only
#' investigators in both lists are available for case assignment.
#'
#' While the default behavior is an inner join, `get_investigators()` can
#' also perform anti-joins with either the scheduled or REDcap investigator list
#' as the primary table. This is useful for debugging and checking that all
#' scheduled investigators are able to have cases assigned.
#'
#' @inheritParams sched_investigators
#'
#' @inheritParams download_redcap_investigators
#'
#' @param quiet Should update messages be suppressed?
#'
#' @return A `tibble` with one row per investigator and an `investigator`
#'   column. If `type = "inner"`, an `id` column is also included.
#'
#' @export
get_investigators <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_NCA_token"),
  quiet = TRUE
) {

  if (!quiet) rlang::inform("Loading investigators...")
  inv_scheduled <- suppressMessages(
    sched_investigators(date = date)
  )
  inv_redcap <- suppressMessages(
    download_redcap_investigators(api_token = api_token)
  )
  if (!quiet) rlang::inform("Returning investigators in both lists...")
  dplyr::inner_join(
    inv_scheduled,
    inv_redcap,
    by = "investigator"
  ) %>%
    dplyr::select("id", "team", "investigator") %>%
    dplyr::arrange(.data[["investigator"]], .data[["id"]]) %>%
    dplyr::distinct(.data[["investigator"]], .keep_all = TRUE)
}
