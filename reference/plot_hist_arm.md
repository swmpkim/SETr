# Generate a histogram of pin readings by arm position

Generate a histogram of pin readings by arm position

## Usage

``` r
plot_hist_arm(
  data,
  set_id = set_id,
  arm_position = arm_position,
  pin_height = pin_height,
  columns = 4,
  scales = "free_y"
)
```

## Arguments

- data:

  a data frame with one row per pin reading

- set_id, arm_position, pin_height:

  unquoted column names in \`data\` for SET ID, arm position, and pin
  height, respectively. Default to \`set_id\`, \`arm_position\`,
  \`pin_height\`; override if your data uses different names.

- columns:

  number of columns you'd like in the faceted plot

- scales:

  passed to \`facet_wrap\` - fixed or free?

## Value

a ggplot object

## Examples

``` r
plot_hist_arm(example_sets)
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```
