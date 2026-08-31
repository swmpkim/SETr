# Plot change between readings, by arm

Plot change between readings, by arm

## Usage

``` r
plot_incr_arm(
  data,
  set = NULL,
  date = date,
  set_id = set_id,
  arm_position = arm_position,
  mean_incr = mean_incr,
  threshold = 25,
  columns = 4,
  pointsize = 2,
  scales = "fixed"
)
```

## Arguments

- data:

  data frame (e.g. \`\$arm\` piece of output from
  \`calc_change_incr()\`) with one row per faceting variable.
  \`mean_incr\` should be an already-calculated field of change since
  previous reading.

- set:

  optional SET ID if you only want to look at one SET; default is to
  graph all SETs

- date, set_id, arm_position, mean_incr:

  unquoted column names in \`data\`. Default to \`date\`, \`set_id\`,
  \`arm_position\`, \`mean_incr\` – the names used by
  \`calc_change_incr()\`'s output; override if your data uses different
  names.

- threshold:

  numeric value for red horizontal lines (at +/- this value); this
  should be a value that would be a meaningful threshold for incremental
  change.

- columns:

  number of columns for faceted output

- pointsize:

  size of points you want (goes into the \`size\` argument of
  \`ggplot2::geom_point\`)

- scales:

  passed to \`facet_wrap\`; same fixed/free options as that function

## Value

a ggplot object

## Examples

``` r
incr_set <- calc_change_incr(example_sets)
plot_incr_arm(incr_set$arm)
#> Warning: Removed 4 rows containing missing values or values outside the scale range
#> (`geom_point()`).

plot_incr_arm(incr_set$arm, threshold = 5, columns = 1)
#> Warning: Removed 4 rows containing missing values or values outside the scale range
#> (`geom_point()`).

plot_incr_arm(incr_set$arm, set = "SET2", threshold = 5)
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).
```
