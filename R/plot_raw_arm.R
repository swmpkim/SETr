#' Plot raw pin measurements, first averaged to arm level, by date
#'
#' @param data a data frame with one row per pin reading, and the following columns, named exactly: date, set_id, arm_position, pin_height
#' @param columns number of columns for the faceted graph
#' @param pointsize size of points for `geom_point()` layer
#' @param sdline logical; include error bars for +/- one standard deviation?
#' @param sdlinesize size for width of error bars
#' @param scales passed to `facet_wrap`; same fixed/free options as that function
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' plot_raw_arm(example_sets)
#' plot_raw_arm(example_sets, columns = 1, pointsize = 3)
#' plot_raw_arm(example_sets, sdline = FALSE)

plot_raw_arm <- function(data, columns = 4, pointsize = 2, sdline = TRUE, sdlinesize = 1, scales = "free_y"){
    data %>%
        dplyr::group_by(.data$set_id, .data$arm_position, .data$date) %>%
        dplyr::summarize(mean = mean(.data$pin_height, na.rm = TRUE),
                  sd = stats::sd(.data$pin_height, na.rm = TRUE)) %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data$mean, color = as.factor(.data$arm_position))) +
        ggplot2::geom_point(size = pointsize) +
        ggplot2::geom_line(alpha = 0.6) +
        {if(sdline) ggplot2::geom_errorbar(ggplot2::aes(x = .data$date,
                                      ymin = .data$mean - .data$sd,
                                      ymax = .data$mean + .data$sd,
                                      color = as.factor(.data$arm_position)
        ),
        size = sdlinesize
        )} +
        ggplot2::facet_wrap(~.data$set_id, ncol = columns, scales = scales) +
        ggplot2::labs(title = 'Pin Height (raw measurement; averaged to arm level)',
             x = 'Date',
             y = 'Mean pin height (mm)',
             color = 'Arm Position') +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = 'bottom')
}
