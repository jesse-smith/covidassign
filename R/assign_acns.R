#' Assign Cases to Scheduled Investigators
#'
#' `assign_acns()` divides unassigned cases among REDcap investigators
#' scheduled to work on `date`. It distributes cases randomly and evenly across
#' investigators until either all are assigned or there are too few cases for
#' another round of even distribution. Any remaining cases are assigned randomly
#' with at most one additional case given to an investigator.
#'
#' @section TODO:
#' \enumerate{
#'   \item Pull school age investigators from team lead sheet
#'   \item Re-factor expressions to be self-documenting (helpers)
#'   \item Add documentation to helpers
#'   \item Create unit tests for all functions in file
#' }
#'
#' @inheritParams get_investigators
#'
#' @param .data Data frame of cases to assign, passed from
#'   \code{\link[covidassign:translate_acns]{translate_acns()}}
#'
#' @return A `tibble` containing the input data with columns `investigator`,
#'   `team`, and `assign_date` added
#'
#' @export
assign_acns <- function(
  .data = translate_acns(),
  date = attr(.data, "date"),
  api_token = Sys.getenv("redcap_NCA_token")
) {

  assigned_cases <- dplyr::bind_rows(
    assign_general(.data, date = date, api_token = api_token),
    assign_school_age(.data, api_token = api_token),
    assign_long_term(.data, api_token = api_token)
  )

  dplyr::select(assigned_cases, -c("school_age", "long_term_care"))
}

#' Assign General Cases to Investigators
#'
#' @inheritParams assign_acns
#'
#' @return The input `.data` with general cases assigned via `investigator`,
#'   `team`, and `assign_date` columns
assign_general <- function(
  .data,
  date = attr(.data, "date"),
  api_token = Sys.getenv("redcap_NCA_token")
) {

  data_general <- dplyr::filter(
    .data,
    !.data[["school_age"]],
    !.data[["long_term_care"]]
  )
  remove(.data)

  investigators <- get_investigators(
    date = date,
    type = "general",
    api_token = api_token
  ) %>%
    dplyr::mutate(id = as.integer(.data[["id"]]))

  if (vec_is_empty(investigators)) {
    investigators <- get_investigators(
      date = date,
      type = "general",
      scheduled_only = FALSE,
      api_token = api_token
    ) %>%
      dplyr::mutate(id = as.integer(.data[["id"]]))
  }

  teams <- download_redcap_template() %>%
    dplyr::filter(.data[["field_name"]] == "team") %>%
    dplyr::pull("select_choices") %>%
    purrr::pluck(1L) %>%
    dplyr::mutate(
      level = as.integer(.data[["level"]]),
      label = stringr::str_to_lower(.data[["label"]])
    ) %>%
    dplyr::transmute(t = set_names(.data[["level"]], .data[["label"]])) %>%
    dplyr::pull(1L)

  # Get number of unassigned cases and investigators
  n_cases <- vec_size(data_general)
  n_investigators <- vec_size(investigators)

  # Get number of investigators needed
  n_reps <- n_cases %/% n_investigators
  if (vec_is_empty(n_reps)) n_reps <- 0L
  n_additional <- n_cases - n_investigators * n_reps

  # Create randomized vector of investigator assignments
  inv_asg <- investigators %>%
    vec_rep(times = n_reps) %>%
    dplyr::bind_rows(dplyr::slice_sample(investigators, n = n_additional)) %>%
    dplyr::slice_sample(prop = 1L)

  # Create randomized `investigator` column and fill `team`, `assign_date`
  dplyr::mutate(
    data_general,
    investigator = inv_asg[["id"]],
    team = vec_slice(teams, i = inv_asg[["team"]]),
    assign_date = lubridate::now()
  )
}

#' Assign School Age Cases to Investigators
#'
#' @inheritParams assign_acns
#'
#' @return The input `.data` with school age cases assigned via `investigator`,
#'   `team`, and `assign_date` columns
assign_school_age <- function(
  .data,
  date = attr(.data, "date"),
  api_token = Sys.getenv("redcap_NCA_token")
) {

  investigators <- get_investigators(
    date = date,
    type = "school",
    api_token = api_token
  ) %>%
    dplyr::mutate(id = as.integer(.data[["id"]]))

  if (vec_is_empty(investigators)) {
    investigators <- get_investigators(
      date = date,
      type = "school",
      scheduled_only = FALSE,
      api_token = api_token
    ) %>%
      dplyr::mutate(id = as.integer(.data[["id"]]))
  }

  data_school_age <- dplyr::filter(.data, .data[["school_age"]])

  if (vec_is_empty(data_school_age)) return(data_school_age)

  # Get number of unassigned cases and investigators
  n_cases <- vec_size(data_school_age)
  n_investigators <- vec_size(investigators)

  # Get number of investigators needed
  n_reps <- n_cases %/% n_investigators
  if (vec_is_empty(n_reps) || is.infinite(n_reps)) n_reps <- 0L
  n_additional <- n_cases - n_investigators * n_reps

  # Create randomized vector of investigator assignments
  inv_asg <- investigators %>%
    vec_rep(times = n_reps) %>%
    dplyr::bind_rows(dplyr::slice_sample(investigators, n = n_additional)) %>%
    dplyr::slice_sample(prop = 1L)

  # Create randomized `investigator` column and fill `team`, `assign_date`
  dplyr::mutate(
    data_school_age,
    investigator = inv_asg[["id"]],
    team = 9L,
    assign_date = lubridate::now()
  )
}

#' Assign Long Term Care Facility Cases to Investigators
#'
#' @inheritParams assign_acns
#'
#' @param assign Character vector of names of investigators to
#'   assign long term care facility cases
#'
#' @return The input `.data` with long term care facility cases assigned via
#'   `investigator`, `team`, and `assign_date` columns
assign_long_term <- function(
  .data,
  assign = "LTCF",
  api_token = Sys.getenv("redcap_NCA_token")
) {

  asg_num <- is.numeric(assign)
  asg_chr <- is.character(assign)

  if (asg_num) {
    assign <- vec_cast(assign, integer())
  } else if (!asg_chr) {
    rlang::abort("`assign` must be character or numeric")
  }

  data_long_term_care <- dplyr::filter(.data, .data[["long_term_care"]])

  investigators <- download_redcap_investigators() %>%
    dplyr::mutate(
      id = as.integer(.data[["id"]]),
      investigator = sched_std_names(.data[["investigator"]])
    ) %>%
    purrr::when(
      asg_num ~ dplyr::filter(., as.integer(.data[["id"]]) %in% assign),
      ~ dplyr::filter(., .data[["investigator"]] %in% sched_std_names(assign))
    )

  coviData::assert_all(
    vec_size(investigators) == vec_size(assign),
    message = paste(
      "All values in `assign` must match exactly one investigator in REDcap"
    )
  )

  # Get number of unassigned cases and investigators
  n_cases <- vec_size(data_long_term_care)
  n_investigators <- vec_size(investigators)

  # Get number of investigators needed
  n_reps <- n_cases %/% n_investigators
  n_additional <- n_cases - n_investigators * n_reps

  # Create randomized vector of investigator assignments
  inv_asg <- investigators %>%
    vec_rep(times = n_reps) %>%
    dplyr::bind_rows(dplyr::slice_sample(investigators, n = n_additional)) %>%
    dplyr::slice_sample(prop = 1L)

  # Create randomized `investigator` column and fill `team`, `assign_date`
  dplyr::mutate(
    data_long_term_care,
    investigator = inv_asg[["id"]],
    team = 10L,
    assign_date = lubridate::now()
  )
}
