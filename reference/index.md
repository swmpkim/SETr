# Package index

## Data import, shaping, and cleaning

- [`import_NERRwide()`](https://nerrscdmo.github.io/SETr/reference/import_NERRwide.md)
  : Import and combine NERR SWMP wide-format data from an Excel workbook
- [`pivot_NERRwide()`](https://nerrscdmo.github.io/SETr/reference/pivot_NERRwide.md)
  : Pivot NERR wide-format SET data to long format
- [`height_to_mm()`](https://nerrscdmo.github.io/SETr/reference/height_to_mm.md)
  : Pin Height to mm
- [`exclude_qaqc()`](https://nerrscdmo.github.io/SETr/reference/exclude_qaqc.md)
  : Exclude flagged readings by setting them to NA

## Calculations

- [`calc_change_incr()`](https://nerrscdmo.github.io/SETr/reference/calc_change_incr.md)
  : Incremental Change Calculations
- [`calc_change_cumu()`](https://nerrscdmo.github.io/SETr/reference/calc_change_cumu.md)
  : Calculate Cumulative Change at a SET

## Plotting functions

- [`plot_raw_pin()`](https://nerrscdmo.github.io/SETr/reference/plot_raw_pin.md)
  : Plot raw pin readings for a single SET, faceted by arm position
- [`plot_raw_arm()`](https://nerrscdmo.github.io/SETr/reference/plot_raw_arm.md)
  : Plot raw pin measurements, first averaged to arm level, by date
- [`plot_incr_pin()`](https://nerrscdmo.github.io/SETr/reference/plot_incr_pin.md)
  : Plot change between readings, by pin
- [`plot_incr_arm()`](https://nerrscdmo.github.io/SETr/reference/plot_incr_arm.md)
  : Plot change between readings, by arm
- [`plot_hist_arm()`](https://nerrscdmo.github.io/SETr/reference/plot_hist_arm.md)
  : Generate a histogram of pin readings by arm position
- [`plot_cumu_arm()`](https://nerrscdmo.github.io/SETr/reference/plot_cumu_arm.md)
  : Make a graph of change over time by arm position
- [`plot_cumu_set()`](https://nerrscdmo.github.io/SETr/reference/plot_cumu_set.md)
  : Make a graph of change over time by SET
- [`plot_rate_comps()`](https://nerrscdmo.github.io/SETr/reference/plot_rate_comps.md)
  : Graphical comparison of SET change rates to SLR

## Datasets

- [`example_sets`](https://nerrscdmo.github.io/SETr/reference/example_sets.md)
  : Example SET data
- [`example_sets_wide`](https://nerrscdmo.github.io/SETr/reference/example_sets_wide.md)
  : Example wide-format NERR SET data
