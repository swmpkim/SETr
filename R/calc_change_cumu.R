#' Calculate Cumulative Change at a SET
#'
#' @param dat a data frame with one row per pin reading, and the following columns, named exactly: date, set_id, arm_position, pin_number, pin_height
#'
#' @return a list of three tibbles: one each for pin, arm, and set level calculations. Pin level change is calculated first, as the difference between a pin reading and the reading from the earliest date at that set_id--arm--pin. If the first reading was NA, the entire pin's cumulative readings will be NA. The column name in the $pin tibble is "cumu". For every date of a pin reading, this calculated value will exist. On the first date, it is 0. Cumulative pin changes are then averaged to the arm position level on each date, excluding NAs. St Deviation and St Error are also calculated. There is one calculated value for every arm on every reading date. These columns in the $arm tibble are "mean_cumu", "sd_cumu", and "se_cumu". The cumulative arm changes are then averaged to the SET level, also with st dev and st err. There is one calculated value for every SET on every reading date. The columns in the $set tibble are again "mean_cumu", "sd_cumu", and "se_cumu".
#'
#' @export
#'
#' @examples
#' calc_change_cumu(example_sets)
#'
calc_change_cumu <- function(dat) {

    ## conditions: have correct columns in data frame
    ## stop and give an informative message if this isn't met
    req_clms <- c("set_id", "arm_position", "pin_number", "pin_height", "date")

    if(sum(req_clms %in% names(dat)) != length(req_clms)){
        stop(paste("Your data frame must have the following columns, with these names, but is missing at least one:", paste(req_clms, collapse = ", ")))
    }


    ## calculations

    # by pin
    change_cumu_pin <- dat %>%
        dplyr::group_by(.data$set_id, .data$arm_position, .data$pin_number) %>%
        dplyr::mutate(cumu = .data$pin_height - .data$pin_height[1]) %>%
        # mutate(cumu = pin_height - pin_height[min(which(!is.na(pin_height)))]) %>% ##### subtract off the first pin reading that's not NA
        dplyr::select(-.data$pin_height) %>%
        dplyr::ungroup()

    # pins averaged up to arms
    change_cumu_arm <- change_cumu_pin %>%
        dplyr::group_by(.data$set_id, .data$arm_position, .data$date) %>%
        dplyr::select(-.data$pin_number) %>%
        dplyr::summarize(mean_cumu = mean(.data$cumu, na.rm = TRUE),
                  sd_cumu = stats::sd(.data$cumu, na.rm = TRUE),
                  se_cumu = stats::sd(.data$cumu, na.rm = TRUE)/sqrt(length(!is.na(.data$cumu)))) %>%
        dplyr::ungroup()

    # arms averaged up to SETs
    change_cumu_set <- change_cumu_arm %>%
        dplyr::group_by(.data$set_id, .data$date) %>%
        dplyr::select(-.data$arm_position, mean_value = .data$mean_cumu) %>%
        dplyr::summarize(mean_cumu = mean(.data$mean_value, na.rm = TRUE),
                  sd_cumu = stats::sd(.data$mean_value, na.rm = TRUE),
                  se_cumu = stats::sd(.data$mean_value, na.rm = TRUE)/sqrt(length(!is.na(.data$mean_value)))) %>%
        dplyr::ungroup()

    return(list(pin = change_cumu_pin, arm = change_cumu_arm, set = change_cumu_set))
}
