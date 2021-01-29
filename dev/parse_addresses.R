#' Parse Address Components from Character Vector in Data Frame
#'
#' `parse_addresses()` is a tailored version of the postmastr package's omnibus
#' \code{\link[postmastr:pm_parse]{pm_parse()}} function. It performs additional
#' standardization before and between steps of the
#' \href{postmastr workflow}{https://slu-opengis.github.io/postmastr/articles/postmastr.html#the-postmastr-workflow}
#' and uses predefined dictionaries intended to parse Shelby County addresses
#' and ignore others. These additional steps and dictionaries were developed
#' using ~330k unique addresses from NBS as training data.
#'
#' @param .data A data frame or data frame extension
#'
#' @param .col `<tidy-select>` A single column containing `character` address
#'   data
#'
#' @param quiet Logical. Should updates messages be shown during parsing?
#'
#' @return `.data` with parsed address columns appended to the end of the
#'   data frame
#'
#' @export
parse_addresses <- function(.data, .col, quiet = FALSE) {

  # Address variable in `.data`
  addr <- coviData::select_colnames(.data, {{ .col }})

  # Ensure only one match
  coviData::assert_cols(.data, {{ addr }}, ptype = character(), n = 1L)

  # Create dictionaries
  if (!quiet) rlang::inform("[01/10] Creating dictionaries...")

  # State dictionary - just TN
  state_dict <- postmastr::pm_dictionary(
    type = "state",
    filter = "TN",
    append = postmastr::pm_append(
      type = "state",
      input = c("47", "TENN"),
      output = rep("TN", times = 2L)
    ),
    case = "upper"
  )

  # City dictionary - just Shelby Co cities/communities
  city_dict <- list(
    ARLINGTON = "ARLINGTON",
    BARTLETT = "BARTLETT",
    BRUNSWICK = "BRUNSWICK",
    COLLIERVILLE = c("COLLIERVILLE", "COLLIER VILLE"),
    CORDOVA = "CORDOVA",
    EADS = "EADS",
    ELLENDALE = "ELLENDALE",
    GERMANTOWN = c("GERMANTOWN", "GERMAN TOWN", "GTOWN"),
    `HICKORY HILL` = "HICKORY HILL",
    LAKELAND = c("LAKELAND", "LAKE LAND"),
    MEMPHIS = c("MEMPHIS", "MPHS", "MEM HIS", "MEM"),
    MILLINGTON = "MILLINGTON",
    ROSEMARK = "ROSEMARK"
  ) %>%
    purrr::map2(names(.), ~ set_names(.x, rep(.y, vec_size(.x)))) %>%
    purrr::flatten_chr() %>%
    tibble::as_tibble_col(column_name = "input") %>%
    dplyr::mutate(
      output = names(.data[["input"]]),
      city_dict = postmastr::pm_append(
        type = "city",
        input = .data[["input"]],
        output = .data[["output"]]
      )
    ) %>%
    dplyr::pull(.data[["city_dict"]])

  # Directional dictionary
  directional_dict <- postmastr::pm_dictionary(
    type = "directional",
    append = postmastr::pm_append(
      type = "directional",
      input = "SO",
      output = "S"
    ),
    case = "upper"
  )

  # Street suffix dictionary
  suffix_dict <- postmastr::pm_dictionary(
    type = "suffix",
    append = postmastr::pm_append(
      type = "suffix",
      input = c("CI", "CR"),
      output = rep("Cir", 2L)
    ),
    case = "upper"
  )

  # Create data for re-combining address fields later
  if (!quiet) rlang::inform("[02/10] Preparing data...")
  data_id <- .data %>%
    dplyr::mutate(pm_address_temp = std_addr(.data[[addr]])) %>%
    postmastr::pm_identify(var = "pm_address_temp")

  # Parse address fields
  data_id %>%
    postmastr::pm_prep(var = "pm_address_temp", type = "street") %T>%
    {if (!quiet) rlang::inform("[03/10] Parsing ZIP codes...")} %>%
    postmastr::pm_postal_parse() %T>%
    {if (!quiet) rlang::inform("[04/10] Parsing states...")} %>%
    prep_state() %>%
    postmastr::pm_state_parse(dictionary = state_dict) %T>%
    {if (!quiet) rlang::inform("[05/10] Parsing cities...")} %>%
    prep_city() %>%
    postmastr::pm_city_parse(dictionary = expand_city_dict(., city_dict)) %T>%
    {if (!quiet) rlang::inform("[06/10] Parsing house numbers...")} %>%
    prep_house(city_dict = city_dict) %>%
    postmastr::pm_house_parse() %T>%
    {if (!quiet) rlang::inform("[07/10] Parsing directionals...")} %>%
    squish_addr() %>%
    postmastr::pm_streetDir_parse(dictionary = directional_dict) %T>%
    {if (!quiet) rlang::inform("[08/10] Parsing street suffixes...")} %>%
    squish_addr() %>%
    postmastr::pm_streetSuf_parse(dictionary = suffix_dict) %T>%
    {if (!quiet) rlang::inform("[09/10] Parsing street names...")} %>%
    squish_addr() %>%
    postmastr::pm_street_parse() %T>%
    {if (!quiet) rlang::inform("[10/10] Rebuilding data...")} %>%
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_to_upper)) %>%
    postmastr::pm_replace(source = data_id) %>%
    postmastr::pm_rebuild(output = "full", keep_parsed = "yes") %>%
    dplyr::select(-"pm_address_temp")
}

#' Prepare Data for Parsing by `pm_state_parse()`
#'
#' `prep_state()` is an internal function used by `parse_addresses()`. It
#' removes trailing numeric expressions that are not the Tennessee FIPS code
#' (`"47"`) and "squishes" extraneous whitespace.
#'
#' @param .data The output of `pm_prep()`
#'
#' @return `.data` with the `pm.address` variable prepared for parsing by
#'   `pm_parse_state()`
#'
#' @keywords internal
prep_state <- function(.data) {
  dplyr::mutate(
    .data,
    pm.address = .data[["pm.address"]] %>%
      stringr::str_squish() %>%
      stringr::str_remove(
        "([0-9-]{3,}|[0-689-]|(?<!4)7|[0-35-9-][0-689-])$"
      ) %>%
      stringr::str_squish()
  )
}

#' Prepare Data for Parsing by `pm_city_parse()`
#'
#' `prep_city()` is an internal function used by `parse_addresses()`. It
#' re-parses postal codes and states from the data to clear any double entry
#' due to multiple fields being concatenated, and it "squishes" extraneous
#' whitespace.
#'
#' @param .data The output of `pm_parse_state()`
#'
#' @return `.data` with the `pm.address` variable prepared for parsing by
#'   `pm_parse_city()`
#'
#' @keywords internal
prep_city <- function(.data) {
  .data %>%
    dplyr::mutate(
      pm.address = stringr::str_squish(.data[["pm.address"]])
    ) %>%
    dplyr::mutate(
      pm.address = postmastr::pm_postal_parse(.) %>%
        prep_state() %>%
        postmastr::pm_state_parse() %>%
        dplyr::pull(.data[["pm.address"]]) %>%
        stringr::str_squish()
    )
}


#' Prepare Data for Parsing by `pm_house_parse()`
#'
#' `prep_house()` is an internal function used by `parse_addresses()`. It first
#' re-parses postal codes, states, and cities to clear any double entry due to
#' multiple fields being concatenated. It then cleans references to
#' apartments/units/rooms/PO Boxes (and associated numeric expressions) and
#' "squishes" extraneous whitespace
#'
#' @param .data The output of `pm_parse_city()`
#'
#' @return `.data` with the `pm.address` variable prepared for parsing by
#'   `pm_parse_house()`
#'
#' @keywords internal
prep_house <- function(.data, city_dict) {

  .data %>%
    dplyr::mutate(
      pm.address = postmastr::pm_postal_parse(.) %>%
        prep_state() %>%
        postmastr::pm_state_parse() %>%
        prep_city() %>%
        postmastr::pm_city_parse(dictionary = city_dict) %>%
        dplyr::pull(.data[["pm.address"]]) %>%
        stringr::str_squish()
    ) %>%
    dplyr::mutate(
      pm.address = .data[["pm.address"]] %>%
        stringr::str_squish() %>%
        stringr::str_remove(pattern = "([0-9]+[ ]?[A-Z]?)$") %>%
        stringr::str_squish() %>%
        stringr::str_remove(pattern = "([#]| NO)$") %>%
        stringr::str_squish() %>%
        stringr::str_remove(pattern = "BO?X$") %>%
        stringr::str_squish() %>%
        stringr::str_remove(pattern = "(P[ ]?O)|(POST OFFICE)$") %>%
        stringr::str_squish() %>%
        stringr::str_remove(pattern = "(RM|ROOM)[ ]?([#]|NO[ ]?[0-9]+)?$") %>%
        stringr::str_squish() %>%
        stringr::str_remove(pattern = "APT?|UNIT$") %>%
        stringr::str_squish() %>%
        stringr::str_remove(pattern = "(RM|ROOM)[ ]?([#]|NO[ ]?[0-9]+)?$") %>%
        stringr::str_squish() %>%
        stringr::str_remove(pattern = "^(RM|ROOM)[ ]?([#]|NO[ ]?[0-9]+)?") %>%
        stringr::str_squish() %>%
        stringr::str_remove(pattern = "^APT?|UNIT") %>%
        stringr::str_squish() %>%
        stringr::str_remove(pattern = "^(RM|ROOM)[ ]?([#]|NO[ ]?[0-9]+)?") %>%
        stringr::str_squish() %>%
        stringr::str_remove(pattern = "^(P[ ]?O)|(POST OFFICE)") %>%
        stringr::str_squish() %>%
        stringr::str_remove(pattern = "^BO?X") %>%
        stringr::str_squish() %>%
        stringr::str_remove(pattern = "^([#]|NO[ ]?[0-9]+)") %>%
        stringr::str_squish() %>%
        stringr::str_remove(pattern = "^[0-9 ]+(?=([ ][0-9]+))") %>%
        stringr::str_squish()
    )
}

#' Expand a City Dictionary with Approximate String Matching
#'
#' `expand_city_dict()` is an internal function used by `parse_addresses()`. It
#' takes an existing dictionary of cities and expands it to match city names
#' within a specified \code{\link[stringdist:stringdist-metrics]{OSA distance}}
#' of the `city.output` values. This is useful for handling typos.
#'
#' @param .data The output of `prep_city()`
#'
#' @param city_dict A city dictionary created by `pm_append()` or
#'   `pm_dictionary()`
#'
#' @param dist_max Maximum OSA distance to consider a match when expanding city
#'   names
#'
#' @return A `tibble` with columns `city.input` holding variants of city names
#'   found in the data and `city.output` holding standardized city names given
#'   by `city_dict`
#'
#' @keywords internal
expand_city_dict <- function(
  .data,
  city_dict,
  dist_max = 2
) {

  if ("city.output" %in% colnames(city_dict)) {
    city_output <- city_dict %>%
      dplyr::distinct() %>%
      dplyr::pull("city.output")

  } else {
    city_output <- city_dict %>%
      dplyr::distinct() %>%
      dplyr::pull("city.input")
  }

  city_input <- city_dict %>%
    dplyr::distinct() %>%
    dplyr::pull("city.input")

  expansion <- .data %>%
    postmastr::pm_city_none(dictionary = city_dict) %>%
    dplyr::select("pm.address") %>%
    dplyr::transmute(
      input = stringr::str_extract(.data[["pm.address"]], pattern = "[^ ]+$"),
      output = .data[["input"]] %>%
        stringdist::amatch(table = {{ city_output }}, maxDist = dist_max) %>%
        {city_output[.]}
    ) %>%
    dplyr::filter(!is.na(.data[["input"]]), !is.na(.data[["output"]])) %>%
    dplyr::bind_rows(
      tibble::tibble(input = city_input, output = city_output)
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data[["output"]], .data[["input"]])

  postmastr::pm_append(
    type = "city",
    input = expansion[["input"]],
    output = expansion[["output"]]
  )

}

#' "Squish" Address Variable Between Parsing Steps in the postmastr workflow
#'
#' `squish_addr()` removes extraneous whitespace from the columns specified by
#' `.cols`; this is typically the `pm.address` variable used in the postmastr
#' workflow.
#'
#' @param .data An intermediate `tibble` in the postmastr workflow
#'
#' @param .cols `<tidy-select>` `character` columns to squish. If any of
#'   these are not of type `character`, they are ignored with a warning.
#'
#' @return `.data` with squished character columns
#'
#' @keywords internal
squish_addr <- function(.data, .cols = "pm.address") {

  # Get selected column names
  cols <- coviData::select_colnames(.data, .cols)

  # Determine whether columns are character variables
  is_chr <- .data %>%
    dplyr::summarize(dplyr::across({{ cols }}, .fns = is.character)) %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::transmute(is_chr = set_names(.data[["value"]], .data[["name"]])) %>%
    dplyr::pull(.data[["is_chr"]])

  # Warn if any are not character variables
  if (!all(is_chr)) {
    not_chr <- cols[!is_chr] %>% set_names(rep("x", times = length(.)))

    rlang::warn(
      paste0(
        "The following columns cannot be squished ",
        "because they are not character variables:\n",
        rlang::format_error_bullets(not_chr)
      )
    )
  }

  # Subset to only character variables
  chr_cols <- cols[is_chr]

  # Squish columns
  dplyr::mutate(.data, dplyr::across({{ chr_cols }}, stringr::str_squish))
}
