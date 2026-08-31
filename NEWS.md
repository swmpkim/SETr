# SETr 0.0.0.9003

* Added `exclude_qaqc()`, to set flagged readings to `NA` based on a QAQC code column, so they drop out of downstream calculations.
* `import_NERRwide()`:
  * Now recognizes both the current `pin_<n>_qaqc` QAQC-code suffix and the legacy `pin_<n>_qaqc_code` suffix (same generalization applied to `pivot_NERRwide()`), so data can be read the same way during the transition between the two conventions.
  * Combines `year`/`month`/`day` columns into a proper `date` column, when all three are present and there isn't already a `date` column.
  * Coerces pin height columns (`pin_<n>_height_<unit>`) to numeric.
  * Fixed a bug where passing more than one sheet name to `sheets` errored (`the condition has length > 1`).
* `plot_rate_comps()`:
  * All internal `ggplot2` calls are now fully package-qualified (`ggplot2::`), rather than relying on `@importFrom` declarations.
  * Fixed a `geom_errorbarh()` deprecation (ggplot2 4.0.0) by switching to `geom_errorbar(orientation = "y")`.
  * Added a `veg_palette` argument: the default `"auto"` keeps today's `"Dark2"` palette for 8 or fewer vegetation categories and automatically switches to a `viridis` scale for more, so extra categories no longer silently lose their color; any RColorBrewer qualitative palette name (or `"viridis"`) can be supplied to override this directly.
* Added `example_sets_wide`, a real (reduced) subset of NERRS SET data in wide format, with its source workbook bundled at `inst/extdata/example_wide.xlsx`. Real, runnable `@examples` using it were added to `import_NERRwide()` and `pivot_NERRwide()`.
* Split data documentation into one file per dataset (`R/example_sets.R`, `R/example_sets_wide.R`), matching the one-file-per-object convention already used for functions.

# SETr 0.0.0.9002

* Generalized `calc_change_cumu()`, `calc_change_incr()`, and all `plot_*()` functions to accept custom column names via tidy evaluation, with defaults matching the previous hardcoded names so existing calls are unaffected.  
* Renamed `calc_change_cumu()`'s first argument from `dat` to `data`, for consistency with the rest of the package.  
* Fixed a latent bug in `plot_incr_pin()`/`plot_incr_arm()` where faceting relied on an unquoted bare column name instead of proper tidy evaluation.  
* Added a dark gray reference line at zero to `plot_incr_pin()` and `plot_incr_arm()`.  
* Replaced all internal `magrittr` pipes (`%>%`) with the base R pipe (`|>`); dropped the `magrittr` dependency and raised the minimum required R version to 4.1.  
* Fixed several `ggplot2` deprecation warnings (`size` argument renamed to `linewidth` for line-based geoms).  
* Fixed a `tidyselect` deprecation warning from using `.data` inside `dplyr::across()`.  
* Removed unused `@importFrom` declarations now that every call to another package's function is fully qualified (`pkg::fun()`).  
* Added test coverage for all `plot_*()` functions (previously untested) and expanded coverage of `calc_change_cumu()`/`calc_change_incr()` to include the new custom-column-name behavior.  

# SETr 0.0.0.9001

* Added a `NEWS.md` file to track changes to the package.  
* Correction to `calc_change_cumu()`. Prior version used position to subtract off first pin reading from the rest, but had not first arranged by date - so the wrong reading could be subtracted. This version incorporates arranging so should be correct.  
