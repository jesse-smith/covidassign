#' @rdname asg_schedule
#'
#' @export
asg_schedule_by_cycle <- function(
  cycle = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  start = Sys.Date(),
  end = Sys.Date() + 29L,
  anchor = start
) {

  vctrs::vec_assert(cycle, ptype = logical())

  start  <- lubridate::as_date(start)
  end    <- lubridate::as_date(end)
  anchor <- lubridate::as_date(anchor)

  if (end < start) {
    start_switched <- end
    to_switched    <- start

    start <- start_switched
    end   <- to_switched

    remove(start_switched, to_switched)
  } else if (start == end) {
    rlang::abort("`start` must not equal `end`")
  }

  cycle_length <- vctrs::vec_size(cycle)

  times_start <- abs(anchor - start) %>%
    divide_by(cycle_length) %>%
    ceiling() %>%
    as.integer()

  times_to <- abs(anchor - end) %>%
    divide_by(cycle_length) %>%
    ceiling() %>%
    as.integer()

  start_2_anchor <- rep(cycle, times_start)
  anchor_2_to <- rep(cycle, times_to)

  if (anchor <= start) {
    # `anchor` is also less than `end`
    start_temp <- anchor
    to_temp <- anchor + (cycle_length * times_to - 1L)
    scheduled <- anchor_2_to
  } else if (anchor >= end) {
    # `anchor` is also greater than `start`
    start_temp <- anchor - (cycle_length * times_start - 1L)
    to_temp <- anchor
    scheduled <- start_2_anchor
  } else {
    start_temp <- anchor - (cycle_length * times_start - 1L)
    to_temp    <- anchor + (cycle_length * times_to - 1L)
    scheduled  <- c(start_2_anchor, anchor_2_to[2:vctrs::vec_size(anchor_2_to)])
  }

  dplyr::tibble(
    date = seq(start_temp, to_temp, by = 1L),
    day = weekdays(date),
    scheduled = scheduled
  ) %>%
    dplyr::filter(dplyr::between(.data[["date"]], start, end))
}
