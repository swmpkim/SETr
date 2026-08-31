# Make a graph of change over time by SET

x-axis is date; y-axis is the average of the 36 pin heights' difference
from baseline (first measurement). One facet per SET id.

## Usage

``` r
plot_cumu_set(
  data,
  date = date,
  set_id = set_id,
  mean_cumu = mean_cumu,
  columns = 4,
  pointsize = 3.5,
  scales = "fixed",
  smooth = TRUE,
  lty_smooth = 5
)
```

## Arguments

- data:

  data frame (e.g. \`\$set\` piece of output from
  \`calc_change_cumu()\`) with one row per faceting variable.
  \`mean_cumu\` should be an already-calculated field of change since
  baseline.

- date, set_id, mean_cumu:

  unquoted column names in \`data\`. Default to \`date\`, \`set_id\`,
  \`mean_cumu\` – the names used by \`calc_change_cumu()\`'s output;
  override if your data uses different names.

- columns:

  number of columns you want in the faceted output

- pointsize:

  size of points you want (goes into the \`size\` argument of
  \`ggplot2::geom_point\`)

- scales:

  passed to \`facet_wrap\`; same fixed/free options as that function

- smooth:

  do you want a linear regression plotted on top?

- lty_smooth:

  type of line (1 = solid; 2 and 5 = dashed; normal line types)

## Value

a ggplot object

## Examples

``` r
cumu_set <- calc_change_cumu(example_sets)
plot_cumu_set(cumu_set$set)
#> `geom_smooth()` using formula = 'y ~ x'

plot_cumu_set(cumu_set$set, columns = 1, pointsize = 2, smooth = FALSE)
```
