# # Unfinished
# approx_join <- function(
#   x,
#   y,
#   by = NULL,
#   max_dist = 2L,
#   mode = c("inner", "left", "right", "full", "semi", "anti"),
#   ignore_case = FALSE,
#   distance_col = NULL,
#   na_matches = c("na", "never")
# ) {
#   # Check inputs
#   coviData::assert_dataframe(x)
#   coviData::assert_dataframe(y)
#   max_dist <- vctrs::vec_cast(max_dist, to = integer())[[1L]]
#   mode <- rlang::arg_match(mode)[[1L]]
#   ignore_case <- vctrs::vec_assert(ignore_case, ptype = logical())[[1L]]
#   if (!is.null(distance_col)) {
#     distance_col <- vctrs::vec_assert(distance_col, ptype = character())[[1L]]
#   }
#   na_matches <- rlang::arg_match(na_matches)[[1L]]
#
#   # Check `by`
#
#   # Get columns in each data frame selected by `by`
#   x_by <- coviData::select_colnames(x, {{ by }}) %>% vec_sort()
#   y_by <- coviData::select_colnames(y, {{ by }}) %>% vec_sort()
#
#   # Check that `.data` columns are character
#   coviData::assert_cols(x, {{ x_by }}, ptype = character(), n = vec_size(x_by))
#   coviData::assert_cols(y, {{ y_by }}, ptype = character(), n = vec_size(y_by))
#
#   # Make sure the same columns are matched
#   coviData::assert_all(
#     all(x_by == y_by),
#     message = "`by` must match the same variables in `.data` and `.table`"
#   )
#
#   # Create fuzzy stringdist join function
#   join_fn <- switch(
#     na_matches,
#     "na" = approx_join_na_na,
#     "never" = approx_join_na_never,
#     # "always" = approx_join_na_always,
#     rlang::abort("`na_matches` must be 'na' or 'never'")
#   )
#
#   join <- purrr::partial(
#     join_fn,
#     by = !!x_by,
#     max_dist = !!max_dist,
#     mode = !!mode,
#     ignore_case = !!ignore_case,
#     distance_col = !!distance_col
#   )
#
#   join(x, y)
# }
#
#
# approx_join_na_na <- function(x, y, by, max_dist, mode, ...) {
#   fuzzyjoin::stringdist_join(
#     x = replace_na_chr(x),
#     y = replace_na_chr(y),
#     by = by,
#     max_dist = max_dist,
#     mode = mode,
#     ...
#   ) %>%
#     replace_na_chr(invert = TRUE)
# }
#
# approx_join_na_never <- function(x, y, by, max_dist, mode, ...) {
#
#   # Create function for generating random strings
#   runif_chr <- purrr::partial(
#     sample,
#     x = !!c(as.character(0:9), letters, LETTERS),
#     ... = ,
#     replace = TRUE,
#     prob = NULL
#   )
#
#   # Generate `NA_character_` replacement w/ random head and tail
#   na_chr1 <- runif_chr(max_dist + 1L) %>%
#     append("_NA_character_") %>%
#     append(runif_chr(max_dist + 1L)) %>%
#     paste0(collapse = "")
#
#   na_chr2 <- runif_chr((max_dist + 1L) * 2L) %>%
#     append("_NA_character_") %>%
#     append(runif_chr((max_dist + 1L) * 2L)) %>%
#     paste0(collapse = "")
#
#   fuzzyjoin::stringdist_join(
#     x = replace_na_chr(x, na_chr = na_chr1),
#     y = replace_na_chr(y, na_chr = na_chr2),
#     by = by,
#     max_dist = max_dist,
#     mode = mode,
#     ...
#   ) %>%
#     # `invert = TRUE` converts anything containing `NA_character_` to missing
#     replace_na_chr(invert = TRUE)
# }
#
# replace_na_chr <- function(x, na_chr = "NA_character_", invert = FALSE) {
#
#   # Check `invert` - must be scalar TRUE or scalar FALSE
#   coviData::assert_any(
#     rlang::is_true(invert),
#     rlang::is_false(invert),
#     message = "`invert` must be TRUE or FALSE"
#   )
#
#   # Create function for replacing in atomic character vectors
#   replace_atomic <- invert %>%
#     ifelse(stringr::str_replace, stringr::str_replace_na) %>%
#     purrr::partial(replacement = !!ifelse(invert, na_chr, "NA_character_")) %>%
#     purrr::when(invert ~ purrr::partial(., pattern = !!na_chr), ~ .)
#
#   # Use atomic replacement if input is a character vector, else use with dplyr
#   if (rlang::is_atomic(x) && is.character(x)) {
#     .data %>%
#       as.character() %>%
#       replace_atomic()
#   } else if (is.data.frame(x)) {
#     dplyr::mutate(x, dplyr::across(where(is.character), replace_atomic))
#   } else {
#     rlang::abort("`x` must be an atomic character vector or a data frame")
#   }
# }
#
# intersect_colnames <- function(.data, ...) {
#
#   # If `.data` is not a bare list, convert to one
#   if (!rlang::is_bare_list(.data)) {
#     .data <- list(.data)
#   }
#
#   # Check that `.data` is a list of data frames
#   coviData::assert(
#     all(purrr::map_lgl(.data, ~ is.data.frame(.x))),
#     message = "`.data` must be a list of data frames"
#   )
#
#   # Loop through data frames - can't use bare dots with `purrr::map()` for some
#   # reason, and can't use `where()` outside a tidyselect context
#   col_names <- list_of(.ptype = character())
#   for (i in vec_seq_along(.data)) {
#     col_names[[i]] <- coviData::select_colnames(.data[[i]], ...)
#   }
#
#   purrr::reduce(col_names, dplyr::intersect) %>%
#     purrr::when(rlang::is_empty(.) ~ NULL, ~ .)
# }
#
# # approx_join_na_always <- function(x, y, by, max_dist, mode, ...) {
# #
# #   # Handle right joins - have to reverse to left, then reverse back at end
# #   if (mode == "right") {
# #     mode  <- "left"
# #     y_tmp <- x
# #     x <- y
# #     y <- x
# #     remove(y_tmp)
# #   }
# #
# #   # Gather `...` into list
# #   dots <- rlang::list2(...)
# #
# #   # Function for creating `by` variables on-the-fly
# #   by_fn <- rlang::new_function(
# #     args = rlang::pairlist2(x = , y = ),
# #     body = rlang::expr({
# #       intersect_colnames(
# #         list(x, y),
# #         !!by & where(~ !any(is.na(.x)))
# #       )
# #     })
# #   )
# #
# #   add_all_na <- rlang::new_function(
# #     args = rlang::pairlist2(x = , by = ),
# #     body = rlang::expr({
# #       dplyr::mutate(
# #         x,
# #         .all_na_ = dplyr::across({{ by }}, is.na) %>%
# #           dplyr::rowwise() %>%
# #           dplyr::transmute(all(dplyr::c_across())) %>%
# #           dplyr::pull(1L)
# #       )
# #     })
# #   )
# #
# #   drop_empty_rows_y <- rlang::new_function(
# #     args = rlang::pairlist2(x = , y = , by = ),
# #     body = rlang::expr({
# #       y_na <- y %>% dplyr::select({{ by }}) %>% is.na()
# #
# #       dplyr::summarize(
# #         x,
# #         dplyr::across({{ by }}, is.na) %>%
# #           dplyr::rowwise() %>%
# #           dplyr::summarize(dplyr::c_across())
# #
# #       )
# #     })
# #   )
# #
# #   # Add row numbers and indicator for all `by` variables are missing
# #   x_id <- x %>%
# #     dplyr::mutate(.id_temp_ = vec_seq_along(x)) %>%
# #     add_all_na()
# #   y_id <- y %>%
# #     dplyr::mutate(.id_temp_ = vec_seq_along(y)) %>%
# #     add_all_na()
# #   remove(x, y)
# #
# #   # Save row numbers where all `by` variables are missing
# #   x_i_na <- x_id %>%
# #     dplyr::filter(.data[[".all_na_"]], y_id[[".all_na_"]]) %>%
# #     dplyr::pull(".id_temp_")
# #   y_i_na <- y_id %>%
# #     dplyr::filter(.data[[".all_na_"]], x_id[[".all_na_"]]) %>%
# #     dplyr::pull(".id_temp_")
# #
# #   # Drop rows where all `by` variables are missing
# #   x_drop_empty <- x_id %>%
# #     dplyr::filter(!.data[[".all_na_"]], !x_id[[".all_na_"]]) %>%
# #     dplyr::select(-".all_na_")
# #   y_drop_empty <- y_id %>%
# #     dplyr::filter(!.data[[".all_na_"]], !y_id[[".all_na_"]]) %>%
# #     dplyr::select(-".all_na_")
# #
# #   # Create function for dropping empty rows in `y`
# #   drop_empty_rows <- rlang::new_function(
# #     args = rlang::pairlist2(x = , y = ),
# #     body = rlang::expr({
# #
# #     })
# #   )
# #
# #   # Create join function with constant arguments filled
# #   join_to_y <- rlang::new_function(
# #     args = rlang::pairlist2(x = ),
# #     body = rlang::expr({
# #       print(by_fn(x, !!y_drop_empty))
# #       fuzzyjoin::stringdist_join(
# #         x = x,
# #         y = !!y_drop_empty,
# #         by = by_fn(x, !!y_drop_empty),
# #         max_dist = !!max_dist,
# #         mode = !!mode,
# #         !!!dots
# #       )
# #     })
# #   )
# #
# #   x_drop_empty %>%
# #     dplyr::nest_by(.data[[".id_temp_"]]) %>%
# #     dplyr::rowwise() %>%
# #     dplyr::summarize(
# #       join_to_y(.data[["data"]])
# #     )
# # }
