#' Create a Path from Components
#'
#' `path_create()` creates a cleaned path from the components specified in `...`
#' and an optional file extension (`ext`). The path is automatically expanded
#' using either the \strong{R} or system definition of the home directory.
#'
#' @inheritParams fs::path
#'
#' @inheritParams path_clean
#'
#' @return The combined and cleaned path in an `fs_path` object, which is a
#'   character vector that also has class `fs_path`
#'
#' @noRd
path_create <- function(..., ext = NULL, home = c("r", "user")) {

  if (rlang::is_null(ext)) {
    ext <- ""
  } else {
    ext %<>%
      stringr::str_remove_all("[ \n\t\r]") %>%
      stringr::str_remove("^[.]")
  }

  fs::path(..., ext = ext) %>% path_clean(home = home)

}

#' Clean a Path with the fs Package
#'
#' `path_clean()` is wrapper around \code{\link[fs:path_tidy]{path_tidy()}},
#' \code{\link[fs:path_expand]{path_expand_r()}}
#' (or \code{\link[fs:path_expand]{path_expand()}}), and
#' \code{\link[fs:path_math]{path_norm()}}. It tidies, then expands, then
#' normalizes the given path(s).
#'
#' @inheritParams fs::path_tidy
#'
#' @param home The home directory definition to use when expanding a path. The
#'   default is to use \strong{R}'s definition, but the system definition of the
#'   user's home directory can also be used. These are equivalent to calling
#'   `path_expand_r()` or `path_expand()`, respectively.
#'
#' @return The cleaned path(s) in an `fs_path` object, which is a character
#'   vector that also has class `fs_path`
#'
#' @noRd
path_clean <- function(path, home = c("r", "user")) {

  home %<>%
    stringr::str_to_lower() %>%
    stringr::str_remove_all("[ \n\t\r]")

  home <- rlang::arg_match(home)[[1]]

  if (home == "r") {
    fs_path_expand <- fs::path_expand_r
  } else {
    fs_path_expand <- fs::path_expand
  }

  path %>%
    fs::path_tidy() %>%
    fs_path_expand() %>%
    fs::path_norm()

}
