# Plot raw pin measurements, first averaged to arm level, by date

Plot raw pin measurements, first averaged to arm level, by date

## Usage

``` r
plot_raw_arm(
  data,
  date = date,
  set_id = set_id,
  arm_position = arm_position,
  pin_height = pin_height,
  columns = 4,
  pointsize = 3,
  sdline = TRUE,
  sdlinesize = 0.7,
  scales = "free_y"
)
```

## Arguments

- data:

  a data frame with one row per pin reading

- date, set_id, arm_position, pin_height:

  unquoted column names in \`data\` for date, SET ID, arm position, and
  pin height, respectively. Default to \`date\`, \`set_id\`,
  \`arm_position\`, \`pin_height\`; override if your data uses different
  names.

- columns:

  number of columns for the faceted graph

- pointsize:

  size of points for \`geom_point()\` layer

- sdline:

  logical; include error bars for +/- one standard deviation?

- sdlinesize:

  size for width of error bars

- scales:

  passed to \`facet_wrap\`; same fixed/free options as that function

## Value

a ggplot object

## Examples

``` r
plot_raw_arm(example_sets)

plot_raw_arm(example_sets, columns = 1, pointsize = 3)

plot_raw_arm(example_sets, sdline = FALSE)


# using data with non-default column names
renamed <- example_sets |>
    dplyr::rename(reading_date = date, elevation_mm = pin_height)
plot_raw_arm(renamed, "SET1", date = reading_date, pin_height = elevation_mm)
```
