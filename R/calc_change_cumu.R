#' Calculate Cumulative Change at a SET
#'
#' @param data a data frame with one row per pin reading
#' @param date,set_id,arm_position,pin_number,pin_height unquoted column names in `data`. Default to `date`, `set_id`, `arm_position`, `pin_number`, `pin_height`; override if your data uses different names. Internally, columns are renamed to these canonical names before calculating, so the output's column names are unaffected by this argument.
#'
#' @return a list of three tibbles: one each for pin, arm, and set level calculations. Pin level change is calculated first, as the difference between a pin reading and the reading from the earliest date at that set_id--arm--pin. If the first reading was NA, the entire pin's cumulative readings will be NA. The column name in the $pin tibble is "cumu". For every date of a pin reading, this calculated value will exist. On the first date, it is 0. Cumulative pin changes are then averaged to the arm position level on each date, excluding NAs. St Deviation and St Error are also calculated. There is one calculated value for every arm on every reading date. These columns in the $arm tibble are "mean_cumu", "sd_cumu", and "se_cumu". The cumulative arm changes are then averaged to the SET level, also with st dev and st err. There is one calculated value for every SET on every reading date. The columns in the $set tibble are again "mean_cumu", "sd_cumu", and "se_cumu".
#'
#' @export
#'
#' @examples
#' calc_change_cumu(example_sets)
#'
#' # using data with non-default column names
#' renamed <- example_sets |>
#'     dplyr::rename(reading_date = date, elevation_mm = pin_height)
#' calc_change_cumu(renamed, date = reading_date, pin_height = elevation_mm)
#'
calc_change_cumu <- function(data,
                              date = date,
                              set_id = set_id,
                              arm_position = arm_position,
                              pin_number = pin_number,
                              pin_height = pin_height) {

    ## conditions: have correct columns in data frame
    ## stop and give an informative message if this isn't met
    req_clms <- c(
        rlang::as_name(rlang::enquo(set_id)),
        rlang::as_name(rlang::enquo(arm_position)),
        rlang::as_name(rlang::enquo(pin_number)),
        rlang::as_name(rlang::enquo(pin_height)),
        rlang::as_name(rlang::enquo(date))
    )

    if(sum(req_clms %in% names(data)) != length(req_clms)){
        stop(paste("Your data frame must have the following columns, but is missing at least one:", paste(req_clms, collapse = ", ")))
    }

    # standardize to canonical names so the calculations below are unaffected
    data <- data |>
        dplyr::rename(
            date = {{ date }},
            set_id = {{ set_id }},
            arm_position = {{ arm_position }},
            pin_number = {{ pin_number }},
            pin_height = {{ pin_height }}
        )

    ## calculations

    # have to make sure to arrange properly so correct pin reading is subtracted off

    # by pin
    change_cumu_pin <- data |>
        dplyr::arrange(.data$set_id, .data$arm_position, .data$pin_number, .data$date) |>
        dplyr::group_by(.data$set_id, .data$arm_position, .data$pin_number) |>
        dplyr::mutate(cumu = .data$pin_height - .data$pin_height[1]) |>
        # mutate(cumu = pin_height - pin_height[min(which(!is.na(pin_height)))]) |> ##### subtract off the first pin reading that's not NA
        dplyr::select(-"pin_height") |>
        dplyr::ungroup()

    # pins averaged up to arms
    change_cumu_arm <- change_cumu_pin |>
        dplyr::group_by(.data$set_id, .data$arm_position, .data$date) |>
        dplyr::select(-"pin_number") |>
        dplyr::summarize(mean_cumu = mean(.data$cumu, na.rm = TRUE),
                  sd_cumu = stats::sd(.data$cumu, na.rm = TRUE),
                  se_cumu = stats::sd(.data$cumu, na.rm = TRUE)/sqrt(sum(!is.na(.data$cumu)))) |>
        dplyr::ungroup()

    # arms averaged up to SETs
    change_cumu_set <- change_cumu_arm |>
        dplyr::group_by(.data$set_id, .data$date) |>
        dplyr::select(-"arm_position", mean_value = "mean_cumu") |>
        dplyr::summarize(mean_cumu = mean(.data$mean_value, na.rm = TRUE),
                  sd_cumu = stats::sd(.data$mean_value, na.rm = TRUE),
                  se_cumu = stats::sd(.data$mean_value, na.rm = TRUE)/sqrt(sum(!is.na(.data$mean_value)))) |>
        dplyr::ungroup()

    return(list(pin = change_cumu_pin, arm = change_cumu_arm, set = change_cumu_set))
}
