#' Download Investigators Listed in the Case Assignment REDcap Project
#'
#' `download_redcap_investigators()` downloads all investigators available
#' for assignment in the \strong{Case Assignment} REDcap project, as well as the
#' `id` assigned to each investigator in REDcap.
#'
#' @param api_token The API token for the Case Assignment REDcap project. The
#' default pulls this from the environment variable `redcap_CA_token`.
#'
#' @return A `tibble` with one row for each investigator and columns for `id`
#'   and `investigator` (name)
#'
#' @export
download_redcap_investigators <- function(
  api_token = Sys.getenv("redcap_NCA_token")
) {
  download_redcap_metadata(api_token) %>%
    dplyr::filter(.data[["field_name"]] == "investigator") %>%
    dplyr::pull("select_choices_or_calculations") %>%
    stringr::str_split(pattern = "\\s*[|]\\s*") %>%
    purrr::flatten_chr() %>%
    dplyr::as_tibble() %>%
    tidyr::separate(
      col = "value",
      into = c("id", "investigator"),
      sep = "\\s*[,]\\s*"
    ) %>%
    dplyr::mutate(
      investigator = sched_std_names(.data[["investigator"]])
    ) %>%
    dplyr::arrange(dplyr::desc(as.integer(.data[["id"]]))) %>%
    dplyr::distinct(.data[["investigator"]], .keep_all = TRUE) %>%
    dplyr::arrange(as.integer(.data[["id"]]))
}
