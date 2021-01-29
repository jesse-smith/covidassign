# match_addresses <- function(
#   .data,
#   .table = tibble::as_tibble(fst::fst(ltcf_addr_path)),
#   by = c("address", "house", "preDir", "street", "streetSuf", "sufDir", "city",
#          "state", "zip"),
#   max_dist = 1L
# ) {
#
#   tibble::tibble(by = by, max_dist = as.integer(max_dist)) %>%
#     dplyr::group_by(.data[["max_dist"]]) %>%
#     dplyr::summarize(
#       match_addr_element(
#         dplyr::mutate(
#           {{ .data }},
#           .id_temp_data_ = vec_seq_along({{ .data }})
#         ),
#         dplyr::mutate(
#           {{ .table }},
#           .id_temp_table_ = vec_seq_along({{ .table }})
#         ),
#         by = .data[["by"]],
#         max_dist = .data[["max_dist"]]
#       )
#     ) %>%
#     dplyr::filter(!is.na(.data[[".distance_temp_"]])) %>%
#     dplyr::ungroup() %>%
#     dplyr::add_count(.data[[".id_temp_.x"]], name = ".n_matches_x_temp_") %>%
#     dplyr::filter(.data[[".n_matches_x_temp_"]] == length(by)) %>%
#     dplyr::distinct(.data[[".id_temp_.x"]], .data[[".id_temp_.y"]], .keep_all = TRUE)
# }
#
# match_addr_element <- function(
#   .data,
#   .table = tibble::as_tibble(fst::fst(ltcf_addr_path)),
#   by = c("address", "house", "preDir", "street", "streetSuf", "sufDir", "city",
#          "state", "zip", "zip4"),
#   max_dist = 1L
# ) {
#
#   by <- by %>% stringr::str_remove("^pm.") %>% {paste0("pm.", .)}
#
#   by_values <- c(
#     "pm.address",
#     "pm.house",
#     "pm.preDir",
#     "pm.street",
#     "pm.streetSuf",
#     "pm.sufDir",
#     "pm.city",
#     "pm.state",
#     "pm.zip",
#     "pm.zip4"
#   )
#
#   coviData::assert_all(
#     all(by %in% by_values),
#     message = paste0(
#       "`by` must be one or more of ", paste0(by_values, collapse = ", "), ", ",
#       "with or without the 'pm.' prefix"
#     )
#   )
#
#   approx_join(
#     x = .data,
#     y = .table,
#     by = by,
#     max_dist = vec_rep(max_dist, vec_size(by)),
#     mode = "right",
#     ignore_case = TRUE,
#     distance_col = ".distance_temp_",
#     na_matches = "na"
#   )
# }
