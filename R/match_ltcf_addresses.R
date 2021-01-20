match_addresses <- function(
  .data,
  .table = tibble::as_tibble(fst::fst(ltcf_addr_path)),
  by = NULL,
  max_dist = 1L,
  ignore_case = TRUE
) {

  .data %>%
    dplyr::group_by()

  tibble::tibble(.rows = NROW(.data)) %>%
    dplyr::mutate(
      pm.house = match_addr_element(
        {{ .data }},
        ltcf_tbl,
        field = "pm.house",
        dist_max = as.double(house),
        skip = rlang::is_false(house)
      ),
      pm.street = match_addr_element(
        {{ .data }},
        ltcf_tbl,
        field = "pm.street",
        dist_max = as.double(street),
        skip = rlang::is_false(street)
      ),
      pm.city = match_addr_element(
        {{ .data }},
        ltcf_tbl,
        field = "pm.city",
        dist_max = as.double(city),
        skip = rlang::is_false(city)
      ),
      pm.state = match_addr_element(
        {{ .data }},
        ltcf_tbl,
        field = "pm.state",
        dist_max = as.double(state),
        skip = rlang::is_false(state)
      ),
      pm.zip = match_addr_element(
        {{ .data }},
        ltcf_tbl,
        field = "pm.zip",
        dist_max = as.double(zip),
        skip = rlang::is_false(zip)
      )
    ) %>%
    dplyr::select(where(is.character)) %>%
    dplyr::left_join(
      ltcf_tbl[c("name", colnames(.))] %>% dplyr::as_tibble(),
      by = colnames(.)
    )
}
