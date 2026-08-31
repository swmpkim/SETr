library(testthat)

## plot_rate_comps() branches on plot_type (1-4) x color_by_veg (TRUE/FALSE),
## assembling a different combination of ggplot2 layers each time. These are
## smoke tests -- confirm each combination returns a ggplot object that
## actually builds (ggplot2 evaluates aes() lazily, so a bad {{ column }}
## reference or missing layer won't error until ggplot_build() is called).
## This function had no test coverage before switching its bare ggplot2
## calls (ggplot(), aes(), geom_point(), etc.) to explicit ggplot2:: calls.

example_rates <- data.frame(
    set_id   = c("SET1", "SET2", "SET3"),
    set_rate = c(3.2, 4.0, 5.4),
    ci_low   = c(3.0, 3.2, 5.2),
    ci_high  = c(3.4, 4.8, 5.6),
    veg      = c("Spartina", "Juncus", "Distichlis"),
    stringsAsFactors = FALSE
)

expect_builds <- function(p) {
    expect_s3_class(p, "gg")
    expect_error(ggplot2::ggplot_build(p), NA)
}

test_that("plot_type = 1 (points only, no CIs) builds", {
    p <- plot_rate_comps(data = example_rates, plot_type = 1,
                         set_ids = set_id, set_ci_low = ci_low, set_ci_high = ci_high,
                         rates = set_rate, comp1 = 3.5, comp1_ci_low = 3.3, comp1_ci_high = 3.7)
    expect_builds(p)
})

test_that("plot_type = 1 with color_by_veg builds", {
    p <- plot_rate_comps(data = example_rates, plot_type = 1, color_by_veg = TRUE,
                         set_ids = set_id, set_ci_low = ci_low, set_ci_high = ci_high,
                         rates = set_rate, comp1 = 3.5, comp1_ci_low = 3.3, comp1_ci_high = 3.7,
                         veg = veg)
    expect_builds(p)
})

test_that("plot_type = 2 (SET CIs only) builds", {
    p <- plot_rate_comps(data = example_rates, plot_type = 2,
                         set_ids = set_id, set_ci_low = ci_low, set_ci_high = ci_high,
                         rates = set_rate, comp1 = 3.5, comp1_ci_low = 3.3, comp1_ci_high = 3.7)
    expect_builds(p)
})

test_that("plot_type = 2 with color_by_veg builds", {
    p <- plot_rate_comps(data = example_rates, plot_type = 2, color_by_veg = TRUE,
                         set_ids = set_id, set_ci_low = ci_low, set_ci_high = ci_high,
                         rates = set_rate, comp1 = 3.5, comp1_ci_low = 3.3, comp1_ci_high = 3.7,
                         veg = veg)
    expect_builds(p)
})

test_that("plot_type = 3 (default; SET + SLR CIs) builds", {
    p <- plot_rate_comps(data = example_rates,
                         set_ids = set_id, set_ci_low = ci_low, set_ci_high = ci_high,
                         rates = set_rate, comp1 = 3.5, comp1_ci_low = 3.3, comp1_ci_high = 3.7)
    expect_builds(p)
})

test_that("plot_type = 3 with color_by_veg builds", {
    p <- plot_rate_comps(data = example_rates, color_by_veg = TRUE,
                         set_ids = set_id, set_ci_low = ci_low, set_ci_high = ci_high,
                         rates = set_rate, comp1 = 3.5, comp1_ci_low = 3.3, comp1_ci_high = 3.7,
                         veg = veg)
    expect_builds(p)
})

test_that("plot_type = 4 (adds a second comparison point) builds", {
    p <- plot_rate_comps(data = example_rates, plot_type = 4,
                         set_ids = set_id, set_ci_low = ci_low, set_ci_high = ci_high,
                         rates = set_rate, comp1 = 3.5, comp1_ci_low = 3.3, comp1_ci_high = 3.7,
                         comp2 = 5.5, comp2_ci_low = 5.0, comp2_ci_high = 6.0)
    expect_builds(p)
})

test_that("plot_type = 4 with color_by_veg builds", {
    p <- plot_rate_comps(data = example_rates, plot_type = 4, color_by_veg = TRUE,
                         set_ids = set_id, set_ci_low = ci_low, set_ci_high = ci_high,
                         rates = set_rate, comp1 = 3.5, comp1_ci_low = 3.3, comp1_ci_high = 3.7,
                         comp2 = 5.5, comp2_ci_low = 5.0, comp2_ci_high = 6.0,
                         veg = veg)
    expect_builds(p)
})

# ---- veg_palette: auto-switching and manual override -----------------------

many_veg_rates <- data.frame(
    set_id   = paste0("SET", 1:10),
    set_rate = seq(2, 6, length.out = 10),
    ci_low   = seq(1.8, 5.8, length.out = 10),
    ci_high  = seq(2.2, 6.2, length.out = 10),
    veg      = paste0("Species", 1:10),  # 10 categories: exceeds Dark2's 8-color max
    stringsAsFactors = FALSE
)

test_that("auto (default) uses Dark2 without warning when veg has 8 or fewer categories", {
    expect_warning(
        p <- plot_rate_comps(data = example_rates, color_by_veg = TRUE,
                             set_ids = set_id, set_ci_low = ci_low, set_ci_high = ci_high,
                             rates = set_rate, comp1 = 3.5, comp1_ci_low = 3.3, comp1_ci_high = 3.7,
                             veg = veg),
        NA
    )
    expect_builds(p)
})

test_that("auto (default) switches to viridis without warning when veg has more than 8 categories", {
    expect_warning(
        p <- plot_rate_comps(data = many_veg_rates, color_by_veg = TRUE,
                             set_ids = set_id, set_ci_low = ci_low, set_ci_high = ci_high,
                             rates = set_rate, comp1 = 3.5, comp1_ci_low = 3.3, comp1_ci_high = 3.7,
                             veg = veg),
        NA
    )
    expect_builds(p)

    # confirm every one of the 10 categories actually got a distinct color, i.e.
    # none were dropped/greyed out the way an overrun Dark2 palette would do
    built <- ggplot2::ggplot_build(p)
    point_layer <- which(vapply(built$plot$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))
    n_colors <- length(unique(built$data[[point_layer[1]]]$colour))
    expect_equal(n_colors, 10)
})

test_that("veg_palette accepts an explicit RColorBrewer palette name", {
    p <- plot_rate_comps(data = example_rates, color_by_veg = TRUE, veg_palette = "Set1",
                         set_ids = set_id, set_ci_low = ci_low, set_ci_high = ci_high,
                         rates = set_rate, comp1 = 3.5, comp1_ci_low = 3.3, comp1_ci_high = 3.7,
                         veg = veg)
    expect_builds(p)
})

test_that("veg_palette accepts an explicit \"viridis\" override with few categories", {
    p <- plot_rate_comps(data = example_rates, color_by_veg = TRUE, veg_palette = "viridis",
                         set_ids = set_id, set_ci_low = ci_low, set_ci_high = ci_high,
                         rates = set_rate, comp1 = 3.5, comp1_ci_low = 3.3, comp1_ci_high = 3.7,
                         veg = veg)
    expect_builds(p)
})
