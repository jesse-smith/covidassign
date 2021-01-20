#' @rdname schedule
#'
#' @export
schedule_by_day <- function(
  cycle = c(
    Sun = FALSE,
    Mon = TRUE,
    Tue = TRUE,
    Wed = TRUE,
    Thu = TRUE,
    Fri = TRUE,
    Sat = FALSE
  ),
  start = Sys.Date(),
  end = Sys.Date() + 29L
) {

  # Check that `days` is a logical with a potential value for each weekday
  vec_assert(cycle, ptype = logical(), size = 7L)

  # Check that `days` is a named vector
  days_are_named <- cycle %>%
    names() %>%
    is.null() %>%
    any() %>%
    not()

  if (!days_are_named) {
    rlang::abort(
      paste0(
        "`calculate_schedule_by_day()` requires `days` ",
        "end be a named logical vector"
      )
    )
  }

  # Standardize weekday names
  day_names <- cycle %>% names() %>% parse_weekday()

  # Check that all names are weekdays
  day_names_are_weekdays <- day_names %>%
    is.na() %>%
    any() %>%
    not()

  if (!day_names_are_weekdays) {
    rlang::abort(
      paste0(
        "`calculate_schedule_by_day()` requires day names ",
        "end be names of weekdays or valid abbreviations thereof"
      )
    )
  }

  # Check that an entry is present for each weekday
  day_names_are_unique <- vec_unique_count(day_names)

  if (!day_names_are_unique) {
    rlang::abort(
      paste0(
        "`calculate_schedule_by_day()` requires exactly one value ",
        "for each day of the week"
      )
    )
  }

  # Calculate Schedule
  start <- lubridate::as_date(start)
  end <- lubridate::as_date(end)

  if (end < start) {
    start_switched <- end
    to_switched    <- start

    start <- start_switched
    end   <- to_switched

    remove(start_switched, to_switched)
  }

  names(cycle) <- day_names

  dplyr::tibble(
    date = seq(start, end, by = 1L),
    day = weekdays(date),
    scheduled = cycle[.data[["day"]]]
  )
}
