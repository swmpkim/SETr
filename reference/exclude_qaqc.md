# Exclude flagged readings by setting them to NA

Sets a value column (\`pin_height\` by default) to \`NA\` for any row
whose QAQC code matches one of the codes supplied in \`codes\`, so those
readings drop out of downstream calculations like
\`calc_change_cumu()\`/ \`calc_change_incr()\` without needing to
manually filter rows yourself.

## Usage

``` r
exclude_qaqc(
  data,
  codes,
  qaqc_code = qaqc_code,
  pin_height = pin_height,
  verbose = TRUE
)
```

## Arguments

- data:

  a data frame with one row per pin reading

- codes:

  character vector of QAQC codes that should trigger exclusion. Matching
  is an exact, case-sensitive string match against \`qaqc_code\`.

- qaqc_code, pin_height:

  unquoted column names in \`data\` for the QAQC code to check and the
  value to exclude. Default to \`qaqc_code\`, \`pin_height\`; override
  if your data uses different names. To also exclude based on an
  arm-level QAQC column, call this function again with \`qaqc_code\`
  pointing at that column (e.g. \`exclude_qaqc(data, codes, qaqc_code =
  arm_qaqc_code)\`).

- verbose:

  logical; print a message with the number of readings excluded? Default
  \`TRUE\`.

## Value

\`data\`, with \`pin_height\` set to \`NA\` wherever \`qaqc_code\`
matched one of \`codes\`.

## Examples

``` r
# example_sets doesn't ship with a qaqc_code column, so add one for
# demonstration -- flag one reading with a made-up "CB" (crab burrow) code
dat <- example_sets
dat$qaqc_code <- "OK"
dat$qaqc_code[3] <- "CB"

exclude_qaqc(dat, codes = "CB")
#> 1 reading(s) excluded (set to NA) based on QAQC code(s): CB
#>          date set_id arm_position pin_number pin_height qaqc_code
#> 1  2010-06-01   SET1            a      pin_1        100        OK
#> 2  2010-06-01   SET1            a      pin_2        106        OK
#> 3  2010-06-01   SET1            a      pin_3         NA        CB
#> 4  2010-06-01   SET1            b      pin_1        110        OK
#> 5  2010-06-01   SET1            b      pin_2        125        OK
#> 6  2010-06-01   SET1            b      pin_3        115        OK
#> 7  2011-06-15   SET1            a      pin_1        105        OK
#> 8  2011-06-15   SET1            a      pin_2        113        OK
#> 9  2011-06-15   SET1            a      pin_3        120        OK
#> 10 2011-06-15   SET1            b      pin_1        117        OK
#> 11 2011-06-15   SET1            b      pin_2        128        OK
#> 12 2011-06-15   SET1            b      pin_3        118        OK
#> 13 2012-06-05   SET1            a      pin_1        112        OK
#> 14 2012-06-05   SET1            a      pin_2        120        OK
#> 15 2012-06-05   SET1            a      pin_3        124        OK
#> 16 2012-06-05   SET1            b      pin_1        125        OK
#> 17 2012-06-05   SET1            b      pin_2        123        OK
#> 18 2012-06-05   SET1            b      pin_3        130        OK
#> 19 2010-06-01   SET2            a      pin_1        142        OK
#> 20 2010-06-01   SET2            a      pin_2        148        OK
#> 21 2010-06-01   SET2            a      pin_3        160        OK
#> 22 2010-06-01   SET2            b      pin_1        152        OK
#> 23 2010-06-01   SET2            b      pin_2        167        OK
#> 24 2010-06-01   SET2            b      pin_3        157        OK
#> 25 2011-06-15   SET2            a      pin_1        147        OK
#> 26 2011-06-15   SET2            a      pin_2        155        OK
#> 27 2011-06-15   SET2            a      pin_3        162        OK
#> 28 2011-06-15   SET2            b      pin_1        159        OK
#> 29 2011-06-15   SET2            b      pin_2        170        OK
#> 30 2011-06-15   SET2            b      pin_3        160        OK
#> 31 2012-06-05   SET2            a      pin_1        154        OK
#> 32 2012-06-05   SET2            a      pin_2        162        OK
#> 33 2012-06-05   SET2            a      pin_3        166        OK
#> 34 2012-06-05   SET2            b      pin_1        167        OK
#> 35 2012-06-05   SET2            b      pin_2        165        OK
#> 36 2012-06-05   SET2            b      pin_3        172        OK
```
