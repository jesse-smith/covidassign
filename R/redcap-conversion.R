#' Convert a Data Frame to JSON for Upload to REDcap
#'
#' `to_json_redcap()` uses \code{\link[jsonlite:toJSON]{toJSON()}} to convert
#' a data frame to a JSON object for import into REDcap. It checks that the
#' size of the resultin object does not exceed `size_limit`, which (by default)
#' is 90% of the maximum allowed upload size in the Shelby County REDcap.
#'
#' @param .data A data frame or data frame extension
#'
#' @param size_limit The maximum allowed size of the resulting JSON object
#'
#' @param ... Additional arguments to pass to
#'   \code{\link[jsonlite:toJSON]{toJSON()}}
#'
#' @return A `json` character object
#'
#' @export
to_json_redcap <- function(.data, size_limit = as.integer(0.9 * 32e6), ...) {

  size_limit <- as.integer(size_limit)

  json_data <- jsonlite::toJSON(.data, ...)
  json_size <- object.size(json_data) %>% as.integer()

  if (json_size > size_limit) {

    size_limit_str <- as.character(size_limit)
    json_size_str <- as.character(json_size) %>%
      stringr::str_pad(width = stringr::str_length(size_limit_str))

    rlang::abort(paste0(
      "Data to upload must be less than `limit` when converted to JSON.\n",
      "limit: ", size_limit_str, " B",
      " size: ", json_size_str, " B"
    ))
  }

  json_data
}

to_tbl_redcap <- function(response) {
  response %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON() %>%
    dplyr::as_tibble()
}
