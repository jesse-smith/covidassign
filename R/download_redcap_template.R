download_redcap_template <- function(
  api_token = Sys.getenv("redcap_NCA_token")
) {
  download_redcap_metadata(api_token) %>%
    dplyr::filter(.data[["field_type"]] != "calc") %>%
    dplyr::select(
      -c("field_label", "field_note", "field_annotation"),
      -c("section_header", "custom_alignment", "question_number")
    ) %>%
    mutate_across_str_empty() %>%
    janitor::remove_empty("cols") %>%
    remove_form_name() %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("required"), ~ !is.na(.x) & .x != "n"),
      dplyr::across(dplyr::starts_with("select_choices"), convert_choices)
    ) %>%
    dplyr::rename(
      select_choices = "select_choices_or_calculations",
      text_validation_type = "text_validation_type_or_show_slider_number"
    )
}

remove_form_name <- function(.data) {
  n_names <- vec_unique_count(dplyr::pull(.data, "form_name"))
  if (n_names <= 1L) dplyr::select(.data, -"form_name") else .data
}

mutate_across_str_empty <- function(.data) {
  dplyr::mutate(
    .data,
    dplyr::across(
      where(is.character),
      stringr::str_replace,
      pattern = "^$",
      replacement = NA_character_
    )
  )
}

convert_choices <- function(x) {

  x <- stringr::str_replace(x, "^$", NA_character_)

  x %>%
    stringr::str_split("\\s*[|]\\s*") %>%
    purrr::map(choice_to_tbl) %>%
    vctrs::as_list_of()
}

choice_to_tbl <- function(x) {

  if (all(rlang::is_na(x))) {
    return(tibble::tibble())
  } else if (all(rlang::is_empty(x))) {
    return(tibble::tibble(level = integer(), label = character()))
  }

  x %>%
    tibble::as_tibble_col() %>%
    tidyr::separate(
      col = "value",
      into = c("level", "label"),
      sep = "\\s*[,]\\s*"
    ) %>%
    dplyr::mutate(level = as.integer(.data[["level"]]))
}
