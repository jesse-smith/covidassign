#' @rdname schedule
#'
#' @export
schedule_by_cycle <- function(
  cycle = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  start = Sys.Date(),
  end = Sys.Date() + 29L,
  anchor = start
) {

  vec_assert(cycle, ptype = logical())

  start  <- lubridate::as_date(start)
  end    <- lubridate::as_date(end)
  anchor <- lubridate::as_date(anchor)

  if (end < start) {
    start_switched <- end
    end_switched   <- start

    start <- start_switched
    end   <- end_switched

    remove(start_switched, end_switched)
  } else if (start == end) {
    rlang::abort("`start` must not equal `end`")
  }

  cycle_length <- vec_size(cycle)

  times_start <- abs(anchor - start) %>%
    divide_by(cycle_length) %>%
    ceiling() %>%
    as.integer()

  times_end <- abs(anchor - end) %>%
    divide_by(cycle_length) %>%
    ceiling() %>%
    as.integer()

  start_to_anchor <- rep(cycle, times_start)
  anchor_to_end <- rep(cycle, times_end)

  if (anchor <= start) {
    # `anchor` is also less than `end`
    start_temp <- anchor
    end_temp <- anchor + (cycle_length * times_end - 1L)
    scheduled <- anchor_to_end
  } else if (anchor >= end) {
    # `anchor` is also greater than `start`
    start_temp <- anchor - (cycle_length * times_start - 1L)
    end_temp <- anchor
    scheduled <- start_to_anchor
  } else {
    start_temp <- anchor - (cycle_length * times_start)
    end_temp    <- anchor + (cycle_length * times_end - 1L)
    scheduled  <- c(start_to_anchor, anchor_to_end)
  }

  dplyr::tibble(
    date = seq(start_temp, end_temp, by = 1L),
    day = weekdays(date),
    scheduled = scheduled
  ) %>%
    dplyr::filter(dplyr::between(.data[["date"]], start, end))
}
