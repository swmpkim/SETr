## code to prepare `example_sets_wide` dataset goes here

# Source file: inst/extdata/example_wide.xlsx
# (also referenced directly in the vignette to demonstrate import_NERRwide())
wide_file <- system.file("extdata", "example_wide.xlsx", package = "SETr")

example_sets_wide <- import_NERRwide(wide_file)

usethis::use_data(example_sets_wide, overwrite = TRUE)
