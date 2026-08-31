#' Plot change between readings, by pin
#'
#' @param data data frame (e.g. `$pin` piece of output from `calc_change_incr()`) with one row per faceting variable. `incr` should be an already-calculated field of change since previous reading.
#' @param set SET ID to graph (required)
#' @param date,set_id,arm_position,pin_number,incr unquoted column names in `data`. Default to `date`, `set_id`, `arm_position`, `pin_number`, `incr` -- the names used by `calc_change_incr()`'s output; override if your data uses different names.
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
#' plot_incr_pin(incr_set$pin, set = "SET1")
#' plot_incr_pin(incr_set$pin, set = "SET1", threshold = 5)
#' plot_incr_pin(incr_set$pin, set = "SET2", threshold = 5, columns = 1)

plot_incr_pin <- function(data, set,
                           date = date,
                           set_id = set_id,
                           arm_position = arm_position,
                           pin_number = pin_number,
                           incr = incr,
                           threshold = 25, columns = 2, pointsize = 2, scales = "fixed"){
    # data needs to be the $pin piece of the output from calc_change_incr
    # names in arguments default to columns used in SETr project
    ggplot2::ggplot(data = dplyr::filter(data, {{ set_id }} == !!set),
                    ggplot2::aes(x = {{ date }}, y = {{ incr }},
               color = as.factor({{ pin_number }}))) +
        ggplot2::geom_hline(yintercept = 0, col = "gray40") +
        ggplot2::geom_point(size = pointsize) +
        ggplot2::geom_hline(yintercept = threshold, col = "red", linewidth = 1) +
        ggplot2::geom_hline(yintercept = -1*threshold, col = "red", linewidth = 1) +
        ggplot2::facet_wrap(ggplot2::vars({{ arm_position }}), ncol = columns, scales = scales) +
        ggplot2::labs(title = paste('Incremental Change by pin at', set),
             subtitle = paste('red lines at +/-', threshold, 'mm'),
             x = 'Date',
             y = 'Change since previous reading (mm)',
             color = 'Pin') +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = 'bottom')
}
