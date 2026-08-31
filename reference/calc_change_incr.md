# Incremental Change Calculations

Incremental Change Calculations

## Usage

``` r
calc_change_incr(
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
between a pin reading and the prior pin reading from that
set_id–arm–pin. The column name in the \$pin tibble is "incr". For every
date of a pin reading, this calculated value will exist or be NA. On the
first date, it is NA. Incremental pin changes are then averaged to the
arm position level on each date, excluding NAs. St Deviation and St
Error are also calculated. There is one calculated value for every arm
on every reading date. These columns in the \$arm tibble are
"mean_incr", "sd_incr", and "se_incr". The cumulative arm changes are
then averaged to the SET level, also with st dev and st err. There is
one calculated value for every SET on every reading date. The columns in
the \$set tibble are again "mean_incr", "sd_incr", and "se_incr". Pin
level calculations are the most helpful for qa/qc, as it is possible to
check for and follow-up on readings that have changed more than a
certain amount (e.g. 25 mm) between readings.

## Examples

``` r
calc_change_incr(example_sets)
#> $pin
#> # A tibble: 36 × 6
#>    date       set_id arm_position pin_number pin_height  incr
#>    <date>     <chr>  <chr>        <chr>           <dbl> <dbl>
#>  1 2010-06-01 SET1   a            pin_1             100    NA
#>  2 2011-06-15 SET1   a            pin_1             105     5
#>  3 2012-06-05 SET1   a            pin_1             112     7
#>  4 2010-06-01 SET1   a            pin_2             106    NA
#>  5 2011-06-15 SET1   a            pin_2             113     7
#>  6 2012-06-05 SET1   a            pin_2             120     7
#>  7 2010-06-01 SET1   a            pin_3             118    NA
#>  8 2011-06-15 SET1   a            pin_3             120     2
#>  9 2012-06-05 SET1   a            pin_3             124     4
#> 10 2010-06-01 SET1   b            pin_1             110    NA
#> # ℹ 26 more rows
#> 
#> $arm
#> # A tibble: 12 × 6
#>    set_id arm_position date       mean_incr sd_incr se_incr
#>    <chr>  <chr>        <date>         <dbl>   <dbl>   <dbl>
#>  1 SET1   a            2010-06-01    NaN      NA      NA   
#>  2 SET1   a            2011-06-15      4.67    2.52    1.45
#>  3 SET1   a            2012-06-05      6       1.73    1   
#>  4 SET1   b            2010-06-01    NaN      NA      NA   
#>  5 SET1   b            2011-06-15      4.33    2.31    1.33
#>  6 SET1   b            2012-06-05      5       8.89    5.13
#>  7 SET2   a            2010-06-01    NaN      NA      NA   
#>  8 SET2   a            2011-06-15      4.67    2.52    1.45
#>  9 SET2   a            2012-06-05      6       1.73    1   
#> 10 SET2   b            2010-06-01    NaN      NA      NA   
#> 11 SET2   b            2011-06-15      4.33    2.31    1.33
#> 12 SET2   b            2012-06-05      5       8.89    5.13
#> 
#> $set
#> # A tibble: 6 × 5
#>   set_id date       mean_incr sd_incr se_incr
#>   <chr>  <date>         <dbl>   <dbl>   <dbl>
#> 1 SET1   2010-06-01     NaN    NA      NA    
#> 2 SET1   2011-06-15       4.5   0.236   0.167
#> 3 SET1   2012-06-05       5.5   0.707   0.5  
#> 4 SET2   2010-06-01     NaN    NA      NA    
#> 5 SET2   2011-06-15       4.5   0.236   0.167
#> 6 SET2   2012-06-05       5.5   0.707   0.5  
#> 

# using data with non-default column names
renamed <- example_sets |>
    dplyr::rename(reading_date = date, elevation_mm = pin_height)
calc_change_incr(renamed, date = reading_date, pin_height = elevation_mm)
#> $pin
#> # A tibble: 36 × 6
#>    date       set_id arm_position pin_number pin_height  incr
#>    <date>     <chr>  <chr>        <chr>           <dbl> <dbl>
#>  1 2010-06-01 SET1   a            pin_1             100    NA
#>  2 2011-06-15 SET1   a            pin_1             105     5
#>  3 2012-06-05 SET1   a            pin_1             112     7
#>  4 2010-06-01 SET1   a            pin_2             106    NA
#>  5 2011-06-15 SET1   a            pin_2             113     7
#>  6 2012-06-05 SET1   a            pin_2             120     7
#>  7 2010-06-01 SET1   a            pin_3             118    NA
#>  8 2011-06-15 SET1   a            pin_3             120     2
#>  9 2012-06-05 SET1   a            pin_3             124     4
#> 10 2010-06-01 SET1   b            pin_1             110    NA
#> # ℹ 26 more rows
#> 
#> $arm
#> # A tibble: 12 × 6
#>    set_id arm_position date       mean_incr sd_incr se_incr
#>    <chr>  <chr>        <date>         <dbl>   <dbl>   <dbl>
#>  1 SET1   a            2010-06-01    NaN      NA      NA   
#>  2 SET1   a            2011-06-15      4.67    2.52    1.45
#>  3 SET1   a            2012-06-05      6       1.73    1   
#>  4 SET1   b            2010-06-01    NaN      NA      NA   
#>  5 SET1   b            2011-06-15      4.33    2.31    1.33
#>  6 SET1   b            2012-06-05      5       8.89    5.13
#>  7 SET2   a            2010-06-01    NaN      NA      NA   
#>  8 SET2   a            2011-06-15      4.67    2.52    1.45
#>  9 SET2   a            2012-06-05      6       1.73    1   
#> 10 SET2   b            2010-06-01    NaN      NA      NA   
#> 11 SET2   b            2011-06-15      4.33    2.31    1.33
#> 12 SET2   b            2012-06-05      5       8.89    5.13
#> 
#> $set
#> # A tibble: 6 × 5
#>   set_id date       mean_incr sd_incr se_incr
#>   <chr>  <date>         <dbl>   <dbl>   <dbl>
#> 1 SET1   2010-06-01     NaN    NA      NA    
#> 2 SET1   2011-06-15       4.5   0.236   0.167
#> 3 SET1   2012-06-05       5.5   0.707   0.5  
#> 4 SET2   2010-06-01     NaN    NA      NA    
#> 5 SET2   2011-06-15       4.5   0.236   0.167
#> 6 SET2   2012-06-05       5.5   0.707   0.5  
#> 
```
