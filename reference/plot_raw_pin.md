# Plot raw pin readings for a single SET, faceted by arm position

Plot raw pin readings for a single SET, faceted by arm position

## Usage

``` r
plot_raw_pin(
  data,
  set,
  date = date,
  set_id = set_id,
  arm_position = arm_position,
  pin_number = pin_number,
  pin_height = pin_height,
  columns = 2,
  pointsize = 2,
  scales = "fixed"
)
```

## Arguments

- data:

  a data frame with one row per pin reading

- set:

  character string for the SET you wish to examine

- date, set_id, arm_position, pin_number, pin_height:

  unquoted column names in \`data\` for date, SET ID, arm position, pin
  number, and pin height, respectively. Default to \`date\`, \`set_id\`,
  \`arm_position\`, \`pin_number\`, \`pin_height\`; override if your
  data uses different names.

- columns:

  number of columns to include in faceted graph

- pointsize:

  size of points; passed to \`geom_point()\`

- scales:

  passed to \`facet_wrap\`; same fixed/free options as that function

## Value

a ggplot object

## Examples

``` r
plot_raw_pin(example_sets, "SET1")

plot_raw_pin(example_sets, "SET1", columns = 1, pointsize = 4)

plot_raw_pin(example_sets, "SET1", scales = "free_y")


# using data with non-default column names
renamed <- example_sets |>
    dplyr::rename(reading_date = date, elevation_mm = pin_height)
plot_raw_pin(renamed, "SET1", date = reading_date, pin_height = elevation_mm)
```
