# Pin Height to mm

turn any pin heights into mm, in a column named pin_height

## Usage

``` r
height_to_mm(data)
```

## Arguments

- data:

  a data frame; must have a column named either \`height_cm\` or
  \`height_mm\`

## Value

the original data frame, but if there was a \`height_cm\` or
\`height_mm\` column, it is now named \`pin_height\`. If original
readings were in cm, they have been transformed into mm.

## Examples

``` r
df <- data.frame(site = c("SET1", "SET2"), height_cm = c(15, 18))
height_to_mm(df)
#>   site pin_height
#> 1 SET1        150
#> 2 SET2        180

df <- data.frame(site = c("SET1", "SET2"), height_mm = c(156, 182))
height_to_mm(df)
#>   site pin_height
#> 1 SET1        156
#> 2 SET2        182
```
