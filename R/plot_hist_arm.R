#' Generate a histogram of pin readings by arm position
#'
#' @param data a data frame with one row per pin reading
#' @param set_id,arm_position,pin_height unquoted column names in `data` for SET ID, arm position, and pin height, respectively. Default to `set_id`, `arm_position`, `pin_height`; override if your data uses different names.
#' @param columns number of columns you'd like in the faceted plot
#' @param scales passed to `facet_wrap` - fixed or free?
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' plot_hist_arm(example_sets)

plot_hist_arm <- function(data,
                           set_id = set_id,
                           arm_position = arm_position,
                           pin_height = pin_height,
                           columns = 4, scales = "free_y"){
    ggplot2::ggplot(data) +
        ggplot2::geom_histogram(ggplot2::aes({{ pin_height }}, fill = as.factor({{ arm_position }})), color = 'black') +
        ggplot2::facet_wrap(ggplot2::vars({{ set_id }}), ncol = columns, scales = scales) +
        ggplot2::labs(title = 'Histogram of raw pin heights by SET',
             subtitle = 'colored by arm position; stacked',
             x = 'Pin Height (mm)',
             fill = 'Arm Position') +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = 'bottom')
}
