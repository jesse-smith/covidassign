validate_redcap_data <- function(
  .data,
  api_token = Sys.getenv("redcap_NCA_token")
) {
  template <- download_redcap_template(api_token)

  # Validate that `.data` variables are a subset of those in the template
  # and all `character`
  template_vars <- paste0("^", template[["field_name"]], "$")
  coviData::assert_cols(
    .data,
    dplyr::matches(template_vars),
    ptype = character(),
    n = vec_size(.data)
  )

  # Subset template to variables in `.data`
  template <- dplyr::filter(
    template,
    .data[["field_name"]] %in% colnames(.data)
  )

  # Validate levels, where applicable
  lvl_template <- template %>%
    dplyr::filter(!purrr::map_lgl(.data[["select_choices"]], vec_is_empty)) %>%
    dplyr::transmute(
      .data[["field_name"]],
      levels = .data[["select_choices"]] %>%
        purrr::map(~ as.character(.x[["level"]])) %>%
        as_list_of()
    ) %>%
    tidyr::pivot_wider(names_from = "field_name", values_from = "levels")

  cols_valid <- .data %>%
    dplyr::select(!!lvl_template[["field_name"]]) %>%
    dplyr::summarize(
      dplyr::across(
        .fns = ~ all(.x %in% c(lvl_template[[dplyr::cur_column()]][[1L]], ""))
      )
    ) %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = "variable",
      values_to = "valid"
    )
  remove(lvl_template)

  # Validate dates, where applicable
  date_template <- template
}
