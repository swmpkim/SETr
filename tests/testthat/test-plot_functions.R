library(testthat)

## Smoke tests for the plot_* functions: confirm they return a ggplot object
## and that the plot actually builds (ggplot2 evaluates aes() lazily, so a
## bad {{ column }} reference won't error until ggplot_build() is called).
## These functions had no test coverage before the switch to tidy-eval
## column arguments, so these also guard against regressions in that change.

# ---- plot_raw_pin -----------------------------------------------------------

test_that("plot_raw_pin builds with default column names", {
    p <- plot_raw_pin(example_sets, "SET1")
    expect_s3_class(p, "gg")
    expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("plot_raw_pin builds with custom column names", {
    renamed <- example_sets |>
        dplyr::rename(reading_date = date, elevation_mm = pin_height)
    p <- plot_raw_pin(renamed, "SET1", date = reading_date, pin_height = elevation_mm)
    expect_s3_class(p, "gg")
    expect_error(ggplot2::ggplot_build(p), NA)
})

# ---- plot_raw_arm -------------------------------------------------------------

test_that("plot_raw_arm builds with default column names", {
    p <- plot_raw_arm(example_sets)
    expect_s3_class(p, "gg")
    expect_error(ggplot2::ggplot_build(p), NA)
})

# ---- plot_hist_arm -------------------------------------------------------------

test_that("plot_hist_arm builds with default column names", {
    p <- plot_hist_arm(example_sets)
    expect_s3_class(p, "gg")
    expect_error(ggplot2::ggplot_build(p), NA)
})

# ---- plot_cumu_arm / plot_cumu_set ---------------------------------------------

test_that("plot_cumu_arm and plot_cumu_set build with default column names", {
    cumu <- calc_change_cumu(example_sets)

    p_arm <- plot_cumu_arm(cumu$arm)
    expect_s3_class(p_arm, "gg")
    expect_error(ggplot2::ggplot_build(p_arm), NA)

    p_set <- plot_cumu_set(cumu$set)
    expect_s3_class(p_set, "gg")
    expect_error(ggplot2::ggplot_build(p_set), NA)
})

test_that("plot_cumu_arm builds with custom column names", {
    cumu <- calc_change_cumu(example_sets)
    renamed <- cumu$arm |> dplyr::rename(reading_date = date, change = mean_cumu)

    p <- plot_cumu_arm(renamed, date = reading_date, mean_cumu = change)
    expect_s3_class(p, "gg")
    expect_error(ggplot2::ggplot_build(p), NA)
})

# ---- plot_incr_pin / plot_incr_arm ---------------------------------------------

test_that("plot_incr_pin and plot_incr_arm build with default column names", {
    incr <- calc_change_incr(example_sets)

    p_pin <- plot_incr_pin(incr$pin, set = "SET1")
    expect_s3_class(p_pin, "gg")
    expect_error(ggplot2::ggplot_build(p_pin), NA)

    p_arm <- plot_incr_arm(incr$arm)
    expect_s3_class(p_arm, "gg")
    expect_error(ggplot2::ggplot_build(p_arm), NA)

    p_arm_one_set <- plot_incr_arm(incr$arm, set = "SET2")
    expect_s3_class(p_arm_one_set, "gg")
    expect_error(ggplot2::ggplot_build(p_arm_one_set), NA)
})
