#' Pivot NERR wide-format SET data to long format
#'
#' Converts wide-format data produced by \code{import_NERRwide()} into long
#' format, with one row per pin, by separately pivoting the pin height
#' columns and the pin qaqc_code columns and rejoining them.
#'
#' @param data A data frame produced by \code{import_NERRwide()}, containing
#'   columns matching \code{pin_<number>_height_<unit>} and
#'   \code{pin_<number>_qaqc_code}.
#'
#' @return A long-format data frame with one row per pin per original row,
#'   including a \code{pin_number} column, a \code{height_mm} or \code{height_cm}
#'   column (matching the unit used in \code{data}), and a \code{qaqc_code}
#'   column.
#'
#' @export
pivot_NERRwide <- function(data) {

    if (!any(grepl("^pin_\\d+_height", names(data)))) {
        stop("`data` does not contain any pin height columns matching 'pin_<number>_height_<unit>'.")
    }

    heights_long <- data |>
        dplyr::select(-tidyselect::matches("^pin_\\d+_qaqc_code")) |>
        tidyr::pivot_longer(
            cols = tidyselect::matches("^pin_\\d+_height"),
            names_to = c("pin_number", ".value"),
            names_pattern = "pin_(\\d+)_(height_.*)"
        )

    qaqc_long <- data |>
        dplyr::select(-tidyselect::matches("^pin_\\d+_height")) |>
        tidyr::pivot_longer(
            cols = tidyselect::matches("^pin_\\d+_qaqc_code"),
            names_to = "pin_number",
            names_pattern = "pin_(\\d+)_qaqc_code",
            values_to = "qaqc_code"
        )

    join_cols <- intersect(names(heights_long), names(qaqc_long))

    dplyr::left_join(heights_long, qaqc_long, by = join_cols)
}
