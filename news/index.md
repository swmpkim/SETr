# Changelog

## SETr 0.1.0

First public release.

### Major

- Added a full package vignette walking through the workflow: importing
  wide NERRS data, reshaping to long format, QA/QC graphs and flag
  exclusion, and cumulative/incremental change calculations.
- Added a [pkgdown](https://pkgdown.r-lib.org/) website.
- Created functions to read and reshape NERR-formatted SET data:
  [`import_NERRwide()`](https://nerrscdmo.github.io/SETr/reference/import_NERRwide.md)
  and
  [`pivot_NERRwide()`](https://nerrscdmo.github.io/SETr/reference/pivot_NERRwide.md).
  These functions recognize both the upcoming `pin_<n>_qaqc` QAQC-code
  suffix and the legacy `pin_<n>_qaqc_code` suffix, so data can be read
  the same way during the transition between the two conventions.
- Generalized
  [`calc_change_cumu()`](https://nerrscdmo.github.io/SETr/reference/calc_change_cumu.md),
  [`calc_change_incr()`](https://nerrscdmo.github.io/SETr/reference/calc_change_incr.md),
  and all `plot_*()` functions to accept custom column names via tidy
  evaluation, with defaults matching the previous hardcoded names so
  existing calls are unaffected.
- Fixed a bug in
  [`calc_change_cumu()`](https://nerrscdmo.github.io/SETr/reference/calc_change_cumu.md)
  where the first pin reading was subtracted off by row position rather
  than by date, so the wrong reading could be subtracted if rows weren’t
  already sorted; rows are now arranged by date before calculating.
- Added
  [`exclude_qaqc()`](https://nerrscdmo.github.io/SETr/reference/exclude_qaqc.md),
  to set flagged readings to `NA` based on a QAQC code column, so they
  drop out of downstream calculations.  
- Added automatic color palette change to
  [`plot_rate_comps()`](https://nerrscdmo.github.io/SETr/reference/plot_rate_comps.md)
  when there are more categories than the default color palette can
  handle.  
- Added `example_sets_wide`, a real (reduced) subset of NERRS SET data
  in wide format, with its source workbook bundled at
  `inst/extdata/example_wide.xlsx`. Real, runnable `@examples` using it
  were added to
  [`import_NERRwide()`](https://nerrscdmo.github.io/SETr/reference/import_NERRwide.md)
  and
  [`pivot_NERRwide()`](https://nerrscdmo.github.io/SETr/reference/pivot_NERRwide.md).  
- Added test coverage for all `plot_*()` functions and expanded coverage
  of
  [`calc_change_cumu()`](https://nerrscdmo.github.io/SETr/reference/calc_change_cumu.md)/[`calc_change_incr()`](https://nerrscdmo.github.io/SETr/reference/calc_change_incr.md)/[`exclude_qaqc()`](https://nerrscdmo.github.io/SETr/reference/exclude_qaqc.md)/[`import_NERRwide()`](https://nerrscdmo.github.io/SETr/reference/import_NERRwide.md)/[`pivot_NERRwide()`](https://nerrscdmo.github.io/SETr/reference/pivot_NERRwide.md)/[`plot_rate_comps()`](https://nerrscdmo.github.io/SETr/reference/plot_rate_comps.md).

### Minor / not visible to user

- Replaced all internal `magrittr` pipes (`%>%`) with the base R pipe
  (`|>`); dropped the `magrittr` dependency and raised the minimum
  required R version to 4.1.  
- Renamed
  [`calc_change_cumu()`](https://nerrscdmo.github.io/SETr/reference/calc_change_cumu.md)’s
  first argument from `dat` to `data`, for consistency with the rest of
  the package.  
- [`plot_rate_comps()`](https://nerrscdmo.github.io/SETr/reference/plot_rate_comps.md):
  - All internal `ggplot2` calls are now fully package-qualified
    (`ggplot2::`), rather than relying on `@importFrom` declarations.
  - Fixed a `geom_errorbarh()` deprecation (ggplot2 4.0.0) by switching
    to `geom_errorbar(orientation = "y")`.
  - Added a `veg_palette` argument: the default `"auto"` keeps today’s
    `"Dark2"` palette for 8 or fewer vegetation categories and
    automatically switches to a `viridis` scale for more, so extra
    categories no longer silently lose their color; any RColorBrewer
    qualitative palette name (or `"viridis"`) can be supplied to
    override this directly.
- Added a dark gray reference line at zero to
  [`plot_incr_pin()`](https://nerrscdmo.github.io/SETr/reference/plot_incr_pin.md)
  and
  [`plot_incr_arm()`](https://nerrscdmo.github.io/SETr/reference/plot_incr_arm.md).  
- Split data documentation into one file per dataset
  (`R/example_sets.R`, `R/example_sets_wide.R`), matching the
  one-file-per-object convention already used for functions.
- Fixed several `ggplot2` deprecation warnings (`size` argument renamed
  to `linewidth` for line-based geoms) and a `tidyselect` deprecation
  warning from using `.data` inside
  [`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html).  
- Removed unused `@importFrom` declarations.
