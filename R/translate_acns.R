translate_acns <- function(
  .data = covidsms::prep_acns(assign = TRUE),
  date = lubridate::today()
) {
  .data %>%
    dplyr::filter(!.data[["duplicate"]]) %>%
    dplyr::select(
      local_id = "pkey",
      report_d = "date_added",
      "first_name",
      "last_name",
      dob = "date_of_birth",
      specimen_date = "test_date",
      phone = "pnumber",
      dplyr::everything()
    ) %>%
    dplyr::mutate(
      address = paste(
        stringr::str_replace_na(.data[["addr1"]], ""),
        stringr::str_replace_na(.data[["addr2"]], ""),
        stringr::str_replace_na(.data[["city"]], ""),
        stringr::str_replace_na(.data[["state"]], ""),
        stringr::str_replace_na(.data[["zip"]], "")
      ) %>% covidsms::std_addr(),
      .before = "school_age"
    ) %>%
    dplyr::select(
      -c("result", "sex", "addr1", "addr2", "city", "state", "zip", "duplicate")
    ) %>%
    janitor::clean_names() %>%
    covidsms::as_date_tbl(date = date)
}

