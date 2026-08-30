#' Plot raw pin readings for a single SET, faceted by arm position
#'
#' @param data a data frame with one row per pin reading
#' @param set character string for the SET you wish to examine
#' @param date,set_id,arm_position,pin_number,pin_height unquoted column names in `data` for date, SET ID, arm position, pin number, and pin height, respectively. Default to `date`, `set_id`, `arm_position`, `pin_number`, `pin_height`; override if your data uses different names.
#' @param columns number of columns to include in faceted graph
#' @param pointsize size of points; passed to `geom_point()`
#' @param scales passed to `facet_wrap`; same fixed/free options as that function
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' plot_raw_pin(example_sets, "SET1")
#' plot_raw_pin(example_sets, "SET1", columns = 1, pointsize = 4)
#' plot_raw_pin(example_sets, "SET1", scales = "free_y")
#'
#' # using data with non-default column names
#' renamed <- example_sets |>
#'     dplyr::rename(reading_date = date, elevation_mm = pin_height)
#' plot_raw_pin(renamed, "SET1", date = reading_date, pin_height = elevation_mm)

plot_raw_pin <- function(data, set,
                          date = date,
                          set_id = set_id,
                          arm_position = arm_position,
                          pin_number = pin_number,
                          pin_height = pin_height,
                          columns = 2, pointsize = 2, scales = "fixed"){
    data |>
        dplyr::filter({{ set_id }} == !!set) |>
        dplyr::group_by({{ set_id }}, {{ arm_position }}, {{ pin_number }}, {{ date }}) |>
        ggplot2::ggplot(ggplot2::aes(x = {{ date }}, y = {{ pin_height }}, col = as.factor({{ pin_number }}))) +
        ggplot2::geom_point(size = pointsize) +
        ggplot2::geom_line(alpha = 0.6) +
        ggplot2::facet_wrap(ggplot2::vars({{ arm_position }}), ncol = columns, scales = scales) +
        ggplot2::labs(title = 'Pin Height (raw measurement)',
             subtitle = rlang::sym(set),
             x = 'Date',
             y = 'Measured pin height (mm)',
             color = 'Pin') +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = 'bottom')
}
