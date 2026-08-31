# Example wide-format NERR SET data

A real (though reduced) subset of NERRS Surface Elevation Table data, in
the wide format read by
[`import_NERRwide()`](https://nerrscdmo.github.io/SETr/reference/import_NERRwide.md)
– one row per arm-position reading on a given date, with one column per
pin. Covers three SETs across the 2012-2016 field seasons; the source
workbook (three sheets, one per SET) ships with the package at
`system.file("extdata", "example_wide.xlsx", package = "SETr")`.

## Usage

``` r
example_sets_wide
```

## Format

A data frame with 228 rows (3 SETs x 19 dates x 4 arm positions) and 26
variables:

- set_id:

  Character, unique name of the SET (matches the sheet it came from)

- date:

  Date, combined from `year`/`month`/`day` by
  [`import_NERRwide()`](https://nerrscdmo.github.io/SETr/reference/import_NERRwide.md)

- year, month, day:

  Character, as read from the source spreadsheet

- reserve:

  Character, reserve code

- arm_position:

  Character, one of four arm positions

- arm_qaqc_code:

  Character, QAQC code for the arm/reading as a whole, if any

- pin_1_height_mm, pin_2_height_mm, pin_3_height_mm, pin_4_height_mm,
  pin_5_height_mm, pin_6_height_mm, pin_7_height_mm, pin_8_height_mm,
  pin_9_height_mm:

  Double, height of each of the 9 pins above the arm, in mm

- pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, pin_4_qaqc_code,
  pin_5_qaqc_code, pin_6_qaqc_code, pin_7_qaqc_code, pin_8_qaqc_code,
  pin_9_qaqc_code:

  Character, QAQC code for each pin, if any (legacy `_qaqc_code` suffix
  – see
  [`pivot_NERRwide()`](https://nerrscdmo.github.io/SETr/reference/pivot_NERRwide.md))

## Source

A reduced subset of Grand Bay NERR SET monitoring data, included with
permission for demonstration purposes. Please contact the reserve if you
are interested in using their real SET data.
