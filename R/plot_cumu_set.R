#' Make a graph of change over time
#'
#' x-axis is date; y-axis is the average of the 36 pin heights' difference from baseline (first measurement). One facet per SET id.
#'
#' @param data data frame (e.g. `$set` piece of output from `calc_change_cumu()`)
#' @param columns number of columns you want in the faceted output
#' @param pointsize size of points you want (goes into the `size` argument of `ggplot2::geom_point`)
#' @param scales free or fixed (goes into `scales` arg of `facet_wrap`)
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

plot_cumu_set <- function(data, columns = 4, pointsize = 3.5, scales = "fixed", smooth = TRUE, lty_smooth = 5){
    # data needs to be the $set piece of the output from calc_change_cumu
    ggplot2::ggplot(data, ggplot2::aes(x = .data$date, y = .data$mean_cumu)) +
        ggplot2::geom_line(col = 'lightsteelblue4') +
        {if(smooth) ggplot2::geom_smooth(se = FALSE, method = 'lm',
                                col = 'steelblue4', lty = lty_smooth, size = 1)} +
        ggplot2::geom_point(shape = 21,
                   fill = 'lightsteelblue1', col = 'steelblue3',
                   size = pointsize, alpha = 0.9) +
        ggplot2::facet_wrap(~.data$set_id, ncol = columns, scales = scales) +
        {if(smooth) ggplot2::labs(title = 'Cumulative Change since first reading',
                         subtitle = 'dashed line is linear regression',
                         x = 'Date',
                         y = 'Change since first reading (mm)')} +
        {if(!smooth) ggplot2::labs(title = 'Cumulative Change since first reading',
                          x = 'Date',
                          y = 'Change since first reading (mm)')} +
        ggplot2::theme_classic()
}
