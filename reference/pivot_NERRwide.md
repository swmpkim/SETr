# Pivot NERR wide-format SET data to long format

Converts wide-format data produced by
[`import_NERRwide()`](https://nerrscdmo.github.io/SETr/reference/import_NERRwide.md)
into long format, with one row per pin, by separately pivoting the pin
height columns and the pin QAQC columns and rejoining them.

## Usage

``` r
pivot_NERRwide(data)
```

## Arguments

- data:

  A data frame produced by
  [`import_NERRwide()`](https://nerrscdmo.github.io/SETr/reference/import_NERRwide.md),
  containing columns matching `pin_<number>_height_<unit>` and either
  `pin_<number>_qaqc` (current) or `pin_<number>_qaqc_code` (legacy).
  Both QAQC naming conventions are recognized so that data can be
  pivoted the same way during the transition between them; a single call
  to this function should not mix the two within the same dataset.

## Value

A long-format data frame with one row per pin per original row,
including a `pin_number` column, a `height_mm` or `height_cm` column
(the unit is detected automatically from whichever is present in
`data`), and a `qaqc_code` column.

## Examples

``` r
pivot_NERRwide(example_sets_wide)
#> # A tibble: 2,052 × 11
#>    set_id  date       year  month day   reserve arm_position arm_qaqc_code
#>    <chr>   <date>     <chr> <chr> <chr> <chr>   <chr>        <chr>        
#>  1 CLMAJ-1 2012-02-29 2012  2     29    GND     1L           NA           
#>  2 CLMAJ-1 2012-02-29 2012  2     29    GND     1L           NA           
#>  3 CLMAJ-1 2012-02-29 2012  2     29    GND     1L           NA           
#>  4 CLMAJ-1 2012-02-29 2012  2     29    GND     1L           NA           
#>  5 CLMAJ-1 2012-02-29 2012  2     29    GND     1L           NA           
#>  6 CLMAJ-1 2012-02-29 2012  2     29    GND     1L           NA           
#>  7 CLMAJ-1 2012-02-29 2012  2     29    GND     1L           NA           
#>  8 CLMAJ-1 2012-02-29 2012  2     29    GND     1L           NA           
#>  9 CLMAJ-1 2012-02-29 2012  2     29    GND     1L           NA           
#> 10 CLMAJ-1 2012-02-29 2012  2     29    GND     3R           NA           
#> # ℹ 2,042 more rows
#> # ℹ 3 more variables: pin_number <chr>, height_mm <dbl>, qaqc_code <chr>
```
