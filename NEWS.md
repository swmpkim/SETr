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
