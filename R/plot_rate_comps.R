#' Graphical comparison of SET change rates to SLR
#'
#' @param data a data frame
#' @param plot_type one of various combinations of points and confidence intervals; default is 3.
#' 1 = basic; points only; no confidence intervals
#' 2 = CIs for SET rates, but not sea level rise (SLR)
#' 3 = CIs for both SETs and SLR
#' 4 = all of the above, plus a second comparison point and CIs
#' @param color_by_veg TRUE or FALSE (the default), for whether the points representing SET elevation change rates should be colored according to dominant vegetation at or around the station (if TRUE, the dominant vegetation must be in a column identified by the `veg` argument of this function)
#' @param set_ids column or vector containing unique SET IDs or names
#' @param set_ci_low column or vector of numbers representing the lower limit of the 95\% confidence interval for the SET's rate of elevation change
#' @param set_ci_high column or vector of numbers representing the upper limit of the 95\% confidence interval for the SET's rate of elevation change
#' @param rates column or vector of numbers representing the point estimates of SET rates of elevation change
#' @param comp1 single number that will make a line on the graph (e.g. long-term Sea Level Rise)
#' @param comp1_ci_low single number representing the lower limit of the 95\% confidence interval for the point estimate `comp1`
#' @param comp1_ci_high single number representing the upper limit of the 95\% confidence interval for the point estimate `comp1`
#' @param comp2 optional; another single point estimate to use for a line on the graph (e.g. 19-year water level change)
#' @param comp2_ci_low optional; a single number representing the lower limit of the 95\% confidence interval for the point estimate `comp2`
#' @param comp2_ci_high optional; a single number representing the upper limit of the 95\% confidence interval for the point estimate `comp2`
#' @param veg column or vector containing names of the dominant vegetation type at each SET
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' example_rates <- data.frame("set_id" = c("SET1", "SET2", "SET3"),
#'                             "set_rate" = c(3.2, 4.0, 5.4),
#'                             "ci_low" = c(3.0, 3.2, 5.2),
#'                             "ci_high" = c(3.4, 4.8, 5.6),
#'                             "veg" = c("Spartina", "Juncus", "Distichlis"))
#'
#' plot_rate_comps(data = example_rates,
#'                 set_ids = set_id,
#'                 set_ci_low = ci_low,
#'                 set_ci_high = ci_high,
#'                 rates = set_rate,
#'                 comp1 = 3.5,
#'                 comp1_ci_low = 3.3,
#'                 comp1_ci_high = 3.7)
#'
#' plot_rate_comps(data = example_rates,
#'                 set_ids = set_id,
#'                 color_by_veg = TRUE,
#'                 set_ci_low = ci_low,
#'                 set_ci_high = ci_high,
#'                 rates = set_rate,
#'                 comp1 = 3.5,
#'                 comp1_ci_low = 3.3,
#'                 comp1_ci_high = 3.7,
#'                 veg = veg)
#'
#' plot_rate_comps(data = example_rates,
#'                 plot_type = 4,
#'                 set_ids = set_id,
#'                 set_ci_low = ci_low,
#'                 set_ci_high = ci_high,
#'                 rates = set_rate,
#'                 comp1 = 3.5,
#'                 comp1_ci_low = 3.3,
#'                 comp1_ci_high = 3.7,
#'                 comp2 = 5.5,
#'                 comp2_ci_low = 5.0,
#'                 comp2_ci_high = 6.0)


plot_rate_comps <- function(data, plot_type = 3, color_by_veg = FALSE,
                            set_ids, set_ci_low, set_ci_high,
                            rates,
                            comp1, comp1_ci_low, comp1_ci_high,
                            comp2 = NULL, comp2_ci_low = NULL, comp2_ci_high = NULL,
                            veg){

    # plot_type: 1 = basic; points only; no confidence intervals
    #            2 = CIs for SET rates, but not sea level rise (SLR)
    #            3 = CIs for both SETs and SLR
    #            4 = all of the above, plus a second comparison point and CIs
    # default is the full plot with CIs, and with points all the same color


    # calculate CI half-widths for plot labeling
    comp1_ci_halfwidth <- (comp1_ci_high - comp1_ci_low) / 2
    comp2_ci_halfwidth <- (comp2_ci_high - comp2_ci_low) / 2


    # assemble the base plot, with axes and lines for 0 and SLR
    #####################################################################
    p <- ggplot() +
        geom_blank(data = data,
                   aes(x = {{rates}},
                       y = {{set_ids}})) +
        geom_vline(aes(xintercept = {{comp1}}),
                   col = "navyblue",
                   size = 1,
                   alpha = 0.9) +
        geom_vline(aes(xintercept = 0),
                   col = "gray70") +
        theme_classic()


    # assemble each piece
    #####################################################################

    # points, not colored by veg
    points_same <- geom_point(data = data,
                              aes(x = {{rates}},
                                  y = {{set_ids}}),
                              size = 3,
                              col = "red3")

    # labels, when CIs are included for both SETs and SLR
    labels_set_slr <- labs(title = "Elevation Change with 95% Confidence Intervals",
                           subtitle = paste0("Local, long-term SLR in blue: ",
                                             {{comp1}}, " +/- ",
                                             comp1_ci_halfwidth, " mm/yr"),
                           x = "Rate of change (mm/yr)",
                           y = "SET")

    # labels when SETs, SLR, and 19-year change are included
    labels_all <- labs(title = "Elevation Change with 95% Confidence Intervals",
                       subtitle = paste0("Long-term SLR, solid line & dark shading: ",
                                         {{comp1}}, " +/- ",
                                         comp1_ci_halfwidth, " mm/yr",
                                         "\n19-yr water level change, dashed line & light shading: ",
                                         {{comp2}}, " +/- ",
                                         comp2_ci_halfwidth, " mm/yr"),
                       x = "Rate of change (mm/yr)",
                       y = "SET")

    # labels, when no CIs are included
    labels_minimal <- labs(title = "Elevation Change",
                           subtitle = paste0("Local SLR in blue: ", {{comp1}}, " mm/yr"),
                           x = "Rate of change (mm/yr)",
                           y = "SET")

    # labels, when CIs are included for SETs but not SLR
    labels_partial_setci <- labs(title = "Elevation Change with 95% Confidence Intervals",
                                 subtitle = paste0("Local SLR in blue: ", {{comp1}}, " mm/yr"),
                                 x = "Rate of change (mm/yr)",
                                 y = "SET")

    # geom to include when CIs are included for SETs
    set_cis <- geom_errorbarh(data = data,
                              aes(y = {{set_ids}},
                                  xmin = {{set_ci_low}},
                                  xmax = {{set_ci_high}}),
                              col = "gray55",
                              size = 1)

    # geom to include when CI is included for SLR
    slr_cis <- geom_rect(aes(ymin = -Inf,
                             ymax = Inf,
                             xmin = {{comp1_ci_low}},
                             xmax = {{comp1_ci_high}}),
                         fill = "#08519c", # formerly navyblue. #08519c is a contender; 0.2 here and 0.1 in comp2; #08306b is another one i like a lot
                         alpha = 0.2)

    # geom to include with point estimate for comp2
    comp2_line <- geom_vline(aes(xintercept = {{comp2}}),
                             col = "navyblue",
                             size = 1,
                             linetype = "dashed",
                             alpha = 0.9)

    # geom to include when point and CI included for comp2
    comp2_cis <- geom_rect(aes(ymin = -Inf,
                               ymax = Inf,
                               xmin = {{comp2_ci_low}},
                               xmax = {{comp2_ci_high}}),
                           fill = "#08519c",     #7bccc4, 0.2
                           alpha = 0.1)


    # geom and labels if points will be colored by dominant vegetation type
    if(color_by_veg){
        # the veg column has to be defined
        # this doesn't work though: stopifnot(exists({{veg}}, data))
        points_veg <- geom_point(data = data,
                                 aes(x = {{rates}},
                                     y = {{set_ids}},
                                     col = {{veg}}),
                                 size = 3)
        colors_veg <- scale_color_brewer(type = "qual", palette = "Dark2")
        labels_veg <- labs(color = "Dominant Vegetation")
    }



    ##### Assemble in different ways
    #####################################################################


    ####################################################################
    ### minimal plot: points only; no confidence intervals
    ####################################################################

    # don't color by veg
    if(plot_type == 1 && !color_by_veg){
        p <- p +
            points_same +
            labels_minimal
    }

    # do color by veg
    if(plot_type == 1 && color_by_veg){
        p <- p +
            points_veg +
            colors_veg +
            labels_minimal +
            labels_veg
    }




    ####################################################################
    # Add in CIs for SETs
    ####################################################################
    # don't color by veg
    if(plot_type == 2 && !color_by_veg){
        p <- p +
            set_cis +
            points_same +
            labels_partial_setci
    }

    # do color by veg
    if(plot_type == 2 && color_by_veg){
        p <- p +
            set_cis +
            points_veg +
            colors_veg +
            labels_partial_setci +
            labels_veg
    }



    ####################################################################
    # CIs for both SETs and SLR
    ####################################################################
    # don't color by veg
    if(plot_type == 3 && !color_by_veg){
        p <- p +
            set_cis +
            slr_cis +
            points_same +
            labels_set_slr
    }


    # do color by veg
    if(plot_type == 3 && color_by_veg){
        p <- p +
            set_cis +
            slr_cis +
            points_veg +
            colors_veg +
            labels_set_slr +
            labels_veg
    }


    ####################################################################
    # Everything including comp2
    ####################################################################
    # don't color by veg
    if(plot_type == 4 && !color_by_veg){
        p <- p +
            set_cis +
            slr_cis +
            comp2_line +
            comp2_cis +
            points_same +
            labels_all
    }


    # do color by veg
    if(plot_type == 4 && color_by_veg){
        p <- p +
            set_cis +
            slr_cis +
            comp2_line +
            comp2_cis +
            points_veg +
            colors_veg +
            labels_all +
            labels_veg
    }


    return(p)

}
