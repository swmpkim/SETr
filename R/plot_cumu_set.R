#' Make a graph of change over time by SET
#'
#' x-axis is date; y-axis is the average of the 36 pin heights' difference from baseline (first measurement). One facet per SET id.
#'
#' @param data data frame (e.g. `$set` piece of output from `calc_change_cumu()`) with one row per faceting variable. `mean_cumu` should be an already-calculated field of change since baseline.
#' @param date,set_id,mean_cumu unquoted column names in `data`. Default to `date`, `set_id`, `mean_cumu` -- the names used by `calc_change_cumu()`'s output; override if your data uses different names.
#' @param columns number of columns you want in the faceted output
#' @param pointsize size of points you want (goes into the `size` argument of `ggplot2::geom_point`)
#' @param scales passed to `facet_wrap`; same fixed/free options as that function
#' @param smooth do you want a linear regression plotted on top?
#' @param lty_smooth type of line (1 = solid; 2 and 5 = dashed; normal line types)
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' cumu_set <- calc_change_cumu(example_sets)
#' plot_cumu_set(cumu_set$set)
#' plot_cumu_set(cumu_set$set, columns = 1, pointsize = 2, smooth = FALSE)

plot_cumu_set <- function(data,
                           date = date,
                           set_id = set_id,
                           mean_cumu = mean_cumu,
                           columns = 4, pointsize = 3.5, scales = "fixed", smooth = TRUE, lty_smooth = 5){
    # data needs to be the $set piece of the output from calc_change_cumu
    ggplot2::ggplot(data, ggplot2::aes(x = {{ date }}, y = {{ mean_cumu }})) +
        ggplot2::geom_line(col = 'lightsteelblue4') +
        {if(smooth) ggplot2::geom_smooth(se = FALSE, method = 'lm',
                                col = 'steelblue4', lty = lty_smooth, linewidth = 1)} +
        ggplot2::geom_point(shape = 21,
                   fill = 'lightsteelblue1', col = 'steelblue3',
                   size = pointsize, alpha = 0.9) +
        ggplot2::facet_wrap(ggplot2::vars({{ set_id }}), ncol = columns, scales = scales) +
        {if(smooth) ggplot2::labs(title = 'Cumulative Change since first reading',
                         x = 'Date',
                         y = 'Change since first reading (mm)')} +
        {if(!smooth) ggplot2::labs(title = 'Cumulative Change since first reading',
                          x = 'Date',
                          y = 'Change since first reading (mm)')} +
        ggplot2::theme_classic()
}
