# Calculate Cumulative Change at a SET

Calculate Cumulative Change at a SET

## Usage

``` r
calc_change_cumu(
  data,
  date = date,
  set_id = set_id,
  arm_position = arm_position,
  pin_number = pin_number,
  pin_height = pin_height
)
```

## Arguments

- data:

  a data frame with one row per pin reading

- date, set_id, arm_position, pin_number, pin_height:

  unquoted column names in \`data\`. Default to \`date\`, \`set_id\`,
  \`arm_position\`, \`pin_number\`, \`pin_height\`; override if your
  data uses different names. Internally, columns are renamed to these
  canonical names before calculating, so the output's column names are
  unaffected by this argument.

## Value

a list of three tibbles: one each for pin, arm, and set level
calculations. Pin level change is calculated first, as the difference
between a pin reading and the reading from the earliest date at that
set_id–arm–pin. If the first reading was NA, the entire pin's cumulative
readings will be NA. The column name in the \$pin tibble is "cumu". For
every date of a pin reading, this calculated value will exist. On the
first date, it is 0. Cumulative pin changes are then averaged to the arm
position level on each date, excluding NAs. St Deviation and St Error
are also calculated. There is one calculated value for every arm on
every reading date. These columns in the \$arm tibble are "mean_cumu",
"sd_cumu", and "se_cumu". The cumulative arm changes are then averaged
to the SET level, also with st dev and st err. There is one calculated
value for every SET on every reading date. The columns in the \$set
tibble are again "mean_cumu", "sd_cumu", and "se_cumu".

## Examples

``` r
calc_change_cumu(example_sets)
#> $pin
#> # A tibble: 36 × 5
#>    date       set_id arm_position pin_number  cumu
#>    <date>     <chr>  <chr>        <chr>      <dbl>
#>  1 2010-06-01 SET1   a            pin_1          0
#>  2 2011-06-15 SET1   a            pin_1          5
#>  3 2012-06-05 SET1   a            pin_1         12
#>  4 2010-06-01 SET1   a            pin_2          0
#>  5 2011-06-15 SET1   a            pin_2          7
#>  6 2012-06-05 SET1   a            pin_2         14
#>  7 2010-06-01 SET1   a            pin_3          0
#>  8 2011-06-15 SET1   a            pin_3          2
#>  9 2012-06-05 SET1   a            pin_3          6
#> 10 2010-06-01 SET1   b            pin_1          0
#> # ℹ 26 more rows
#> 
#> $arm
#> # A tibble: 12 × 6
#>    set_id arm_position date       mean_cumu sd_cumu se_cumu
#>    <chr>  <chr>        <date>         <dbl>   <dbl>   <dbl>
#>  1 SET1   a            2010-06-01      0       0       0   
#>  2 SET1   a            2011-06-15      4.67    2.52    1.45
#>  3 SET1   a            2012-06-05     10.7     4.16    2.40
#>  4 SET1   b            2010-06-01      0       0       0   
#>  5 SET1   b            2011-06-15      4.33    2.31    1.33
#>  6 SET1   b            2012-06-05      9.33    9.81    5.67
#>  7 SET2   a            2010-06-01      0       0       0   
#>  8 SET2   a            2011-06-15      4.67    2.52    1.45
#>  9 SET2   a            2012-06-05     10.7     4.16    2.40
#> 10 SET2   b            2010-06-01      0       0       0   
#> 11 SET2   b            2011-06-15      4.33    2.31    1.33
#> 12 SET2   b            2012-06-05      9.33    9.81    5.67
#> 
#> $set
#> # A tibble: 6 × 5
#>   set_id date       mean_cumu sd_cumu se_cumu
#>   <chr>  <date>         <dbl>   <dbl>   <dbl>
#> 1 SET1   2010-06-01       0     0       0    
#> 2 SET1   2011-06-15       4.5   0.236   0.167
#> 3 SET1   2012-06-05      10     0.943   0.667
#> 4 SET2   2010-06-01       0     0       0    
#> 5 SET2   2011-06-15       4.5   0.236   0.167
#> 6 SET2   2012-06-05      10     0.943   0.667
#> 

# using data with non-default column names
renamed <- example_sets |>
    dplyr::rename(reading_date = date, elevation_mm = pin_height)
calc_change_cumu(renamed, date = reading_date, pin_height = elevation_mm)
#> $pin
#> # A tibble: 36 × 5
#>    date       set_id arm_position pin_number  cumu
#>    <date>     <chr>  <chr>        <chr>      <dbl>
#>  1 2010-06-01 SET1   a            pin_1          0
#>  2 2011-06-15 SET1   a            pin_1          5
#>  3 2012-06-05 SET1   a            pin_1         12
#>  4 2010-06-01 SET1   a            pin_2          0
#>  5 2011-06-15 SET1   a            pin_2          7
#>  6 2012-06-05 SET1   a            pin_2         14
#>  7 2010-06-01 SET1   a            pin_3          0
#>  8 2011-06-15 SET1   a            pin_3          2
#>  9 2012-06-05 SET1   a            pin_3          6
#> 10 2010-06-01 SET1   b            pin_1          0
#> # ℹ 26 more rows
#> 
#> $arm
#> # A tibble: 12 × 6
#>    set_id arm_position date       mean_cumu sd_cumu se_cumu
#>    <chr>  <chr>        <date>         <dbl>   <dbl>   <dbl>
#>  1 SET1   a            2010-06-01      0       0       0   
#>  2 SET1   a            2011-06-15      4.67    2.52    1.45
#>  3 SET1   a            2012-06-05     10.7     4.16    2.40
#>  4 SET1   b            2010-06-01      0       0       0   
#>  5 SET1   b            2011-06-15      4.33    2.31    1.33
#>  6 SET1   b            2012-06-05      9.33    9.81    5.67
#>  7 SET2   a            2010-06-01      0       0       0   
#>  8 SET2   a            2011-06-15      4.67    2.52    1.45
#>  9 SET2   a            2012-06-05     10.7     4.16    2.40
#> 10 SET2   b            2010-06-01      0       0       0   
#> 11 SET2   b            2011-06-15      4.33    2.31    1.33
#> 12 SET2   b            2012-06-05      9.33    9.81    5.67
#> 
#> $set
#> # A tibble: 6 × 5
#>   set_id date       mean_cumu sd_cumu se_cumu
#>   <chr>  <date>         <dbl>   <dbl>   <dbl>
#> 1 SET1   2010-06-01       0     0       0    
#> 2 SET1   2011-06-15       4.5   0.236   0.167
#> 3 SET1   2012-06-05      10     0.943   0.667
#> 4 SET2   2010-06-01       0     0       0    
#> 5 SET2   2011-06-15       4.5   0.236   0.167
#> 6 SET2   2012-06-05      10     0.943   0.667
#> 
```
