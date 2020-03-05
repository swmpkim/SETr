#' Plot raw pin readings for a single SET, faceted by arm position
#'
#' @param data a data frame with one row per pin reading, and the following columns, named exactly: date, set_id, arm_position, pin_number, pin_height
#' @param set character string for the SET you wish to examine
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

plot_raw_pin <- function(data, set, columns = 2, pointsize = 2, scales = "fixed"){
    data %>%
        dplyr::filter(.data$set_id == !!set) %>%
        dplyr::group_by(.data$set_id, .data$arm_position, .data$pin_number, .data$date) %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data$pin_height, col = as.factor(.data$pin_number))) +
        ggplot2::geom_point(size = pointsize) +
        ggplot2::geom_line(alpha = 0.6) +
        ggplot2::facet_wrap(~.data$arm_position, ncol = columns, scales = scales) +
        ggplot2::labs(title = 'Pin Height (raw measurement)',
             subtitle = rlang::sym(set),
             x = 'Date',
             y = 'Measured pin height (mm)',
             color = 'Pin') +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = 'bottom')
}
