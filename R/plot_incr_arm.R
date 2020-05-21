#' Plot change between readings, by arm
#'
#' @param data data frame (e.g. `$arm` piece of output from `calc_change_incr()`) with one row per faceting variable, and the following columns, named exactly: date, set_id, arm_position, mean_incr. `mean_incr` should be an already-calculated field of change since previous reading
#' @param set optional SET ID if you only want to look at one SET; default is to graph all SETs
#' @param threshold numeric value for red horizontal lines (at +/- this value); this should be a value that would be a meaningful threshold for incremental change.
#' @param columns number of columns for faceted output
#' @param pointsize size of points you want (goes into the `size` argument of `ggplot2::geom_point`)
#' @param scales passed to `facet_wrap`; same fixed/free options as that function
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' incr_set <- calc_change_incr(example_sets)
#' plot_incr_arm(incr_set$arm)
#' plot_incr_arm(incr_set$arm, threshold = 5, columns = 1)
#' plot_incr_arm(incr_set$arm, set = "SET2", threshold = 5)


plot_incr_arm <- function(data, set = NULL, threshold = 25, columns = 4,
                          pointsize = 2, scales = "fixed"){
    # data needs to be the $arm piece of the output from calc_change_inc
    if(is.null(set)){
        to_plot <- data
        plot_title <- 'Incremental Change by arm'
    }
    else{
        to_plot <- data %>%
            dplyr::filter(.data$set_id == !!set)
        plot_title <- paste('Incremental Change by arm at', set)
    }

    ggplot2::ggplot(data = to_plot, ggplot2::aes(x = .data$date,
                                          y = .data$mean_incr,
                                          color = as.factor(.data$arm_position))) +
        ggplot2::geom_point(size = pointsize) +
        ggplot2::geom_hline(yintercept = threshold, col = "red", size = 1) +
        ggplot2::geom_hline(yintercept = -1*threshold, col = "red", size = 1) +
        ggplot2::facet_wrap(~set_id, ncol = columns, scales = scales) +
        ggplot2::labs(title = plot_title,
             subtitle = paste('red lines at +/-', threshold, 'mm'),
             x = 'Date',
             y = 'Change since previous reading (mm)',
             color = 'Arm Position') +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = 'bottom')
}
