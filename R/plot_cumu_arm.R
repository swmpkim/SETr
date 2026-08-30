#' Make a graph of change over time by arm position
#'
#' x-axis is date; y-axis is the average of the 9 pin heights' difference from baseline (first measurement) for each arm. One facet per SET id.
#'
#' @param data data frame (e.g. `$arm` piece of output from `calc_change_cumu()`) with one row per faceting variable. `mean_cumu` should be an already-calculated field of change since baseline.
#' @param date,set_id,arm_position,mean_cumu unquoted column names in `data`. Default to `date`, `set_id`, `arm_position`, `mean_cumu` -- the names used by `calc_change_cumu()`'s output; override if your data uses different names.
#' @param columns number of columns you want in the faceted output
#' @param pointsize size of points you want (goes into the `size` argument of `ggplot2::geom_point`)
#' @param scales free or fixed (goes into `scales` arg of `facet_wrap`)
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' cumu_set <- calc_change_cumu(example_sets)
#' plot_cumu_arm(cumu_set$arm)
#' plot_cumu_arm(cumu_set$arm, columns = 1, pointsize = 2)

plot_cumu_arm <- function(data,
                           date = date,
                           set_id = set_id,
                           arm_position = arm_position,
                           mean_cumu = mean_cumu,
                           columns = 4, pointsize = 2, scales = "fixed") {
    # data needs to be the $arm piece of the output from calc_change_cumu
    ggplot2::ggplot(data, ggplot2::aes(x = {{ date }}, y = {{ mean_cumu }}, col = as.factor({{ arm_position }}))) +
        ggplot2::geom_point(size = pointsize) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(ggplot2::vars({{ set_id }}), ncol = columns, scales = scales) +
        ggplot2::labs(title = 'Cumulative Change by arm position',
             x = 'Date',
             y = 'Change since first reading (mm)',
             color = "Arm Position") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = 'bottom')
}
