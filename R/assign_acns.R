#' Assign Cases to Scheduled Investigators
#'
#' `assign_cases()` divides unassigned cases among REDcap investigators
#' scheduled to work on `date`. It distributes cases randomly and evenly across
#' investigators until either all are assigned or there are too few cases for
#' another round of even distribution. Any remaining cases are assigned randomly
#' with at most one additional case given to an investigator.
#'
#' @inheritParams get_investigators
#'
#' @return A `tibble` containing the results of
#'   \code{
#'   \link[covidassign:download_redcap_cases]{download_redcap_cases()}
#'   }, with
#'   `assign_date` and `investigator` filled
#'
#' @export
assign_acns <- function(
  .data = translate_acns(),
  date = lubridate::today(),
  asg_school_age = c("Reed Wagner", "Camry Gipson", "Logan Sell", "Shanna Layrock"),
  asg_long_term_care = "Hawa Abdalla",
  api_token = Sys.getenv("redcap_NCA_token")
) {

  dplyr::bind_rows(
    assign_general(.data, date = date, api_token = api_token),
    assign_school_age(.data, assign = asg_school_age, api_token = api_token),
    assign_long_term_care(
      .data,
      assign = asg_long_term_care,
      api_token = api_token
    )
  ) %>%
    dplyr::select(-c("school_age", "long_term_care"))
}

assign_general <- function(
  .data,
  date = lubridate::today(),
  api_token = Sys.getenv("redcap_NCA_token")
) {

  exclude <- vec_c(
    eval(rlang::fn_fmls(assign_school_age)[["assign"]]),
    eval(rlang::fn_fmls(assign_long_term_care)[["assign"]])
  ) %>% coviData::std_names()

  data_general <- dplyr::filter(
    .data,
    !.data[["school_age"]],
    !.data[["long_term_care"]]
  )
  remove(.data)

  investigators <- get_investigators(date = date, api_token = api_token) %>%
    dplyr::mutate(
      id = as.integer(.data[["id"]]),
      team = stringr::str_to_lower(.data[["team"]])
    ) %>%
    dplyr::filter(!coviData::std_names(.data[["investigator"]]) %in% exclude)

  teams <- download_redcap_template() %>%
    dplyr::filter(field_name == "team") %>%
    dplyr::pull(select_choices) %>%
    purrr::pluck(1L) %>%
    dplyr::mutate(
      level = as.integer(.data[["level"]]),
      label = stringr::str_to_lower(.data[["label"]])
    ) %>%
    dplyr::transmute(t = set_names(.data[["level"]], .data[["label"]])) %>%
    dplyr::pull(1L) %>%
    append(c("school" = NA_integer_))

  # Get number of unassigned cases and investigators
  n_cases <- vec_size(data_general)
  print(n_cases)
  n_investigators <- vec_size(investigators)
  print(n_investigators)

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
    data_general,
    investigator = inv_asg[["id"]],
    team = vec_slice(teams, i = inv_asg[["team"]]),
    assign_date = lubridate::now()
  )
}

assign_school_age <- function(
  .data,
  assign = c("Reed Wagner", "Camry Gipson", "Logan Sell", "Shanna Layrock"),
  api_token = Sys.getenv("redcap_NCA_token")
) {

  asg_num <- is.numeric(assign)
  asg_chr <- is.character(assign)

  if (asg_num) {
    assign <- vec_cast(assign, integer())
  } else if (!asg_chr) {
    rlang::abort("`assign` must be character or numeric")
  }

  data_school_age <- dplyr::filter(.data, .data[["school_age"]])

  if (vec_is_empty(data_school_age)) return(data_school_age)

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
  n_cases <- vec_size(data_school_age)
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
    data_school_age,
    investigator = inv_asg[["id"]],
    team = NA_integer_,
    assign_date = lubridate::now()
  )
}

assign_long_term_care <- function(
  .data,
  assign = "Hawa Abdalla",
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
    team = NA_integer_,
    assign_date = lubridate::now()
  )
}
