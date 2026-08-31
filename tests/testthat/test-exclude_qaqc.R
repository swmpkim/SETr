library(testthat)

make_test_data <- function() {
    data.frame(
        set_id      = c("SET1", "SET1", "SET1"),
        pin_number  = c(1, 2, 3),
        pin_height  = c(100, 110, 120),
        qaqc_code   = c("OK", "CB", "OK"),
        stringsAsFactors = FALSE
    )
}

test_that("sets pin_height to NA for matching codes, leaves other rows untouched", {
    dat <- make_test_data()
    result <- suppressMessages(exclude_qaqc(dat, codes = "CB"))

    expect_equal(result$pin_height, c(100, NA, 120))
    # unmatched columns/rows are otherwise unaffected
    expect_equal(result$qaqc_code, dat$qaqc_code)
})

test_that("matching is exact and case-sensitive", {
    dat <- make_test_data()
    result <- suppressMessages(exclude_qaqc(dat, codes = "cb"))
    expect_equal(result$pin_height, dat$pin_height)  # no match -> nothing excluded

    dat2 <- dat
    dat2$qaqc_code[2] <- "CB EXTRA"
    result2 <- suppressMessages(exclude_qaqc(dat2, codes = "CB"))
    expect_equal(result2$pin_height, dat2$pin_height)  # "CB EXTRA" != "CB" -> no match
})

test_that("multiple codes can be excluded at once", {
    dat <- make_test_data()
    dat$qaqc_code <- c("OK", "CB", "DR")
    result <- suppressMessages(exclude_qaqc(dat, codes = c("CB", "DR")))
    expect_equal(result$pin_height, c(100, NA, NA))
})

test_that("custom column names work via qaqc_code and pin_height arguments", {
    dat <- make_test_data() |>
        dplyr::rename(arm_qaqc_code = qaqc_code, elevation_mm = pin_height)

    result <- suppressMessages(
        exclude_qaqc(dat, codes = "CB", qaqc_code = arm_qaqc_code, pin_height = elevation_mm)
    )
    expect_equal(result$elevation_mm, c(100, NA, 120))
})

test_that("no matches leaves all values untouched", {
    dat <- make_test_data()
    result <- suppressMessages(exclude_qaqc(dat, codes = "NOPE"))
    expect_equal(result$pin_height, dat$pin_height)
})

test_that("verbose = TRUE messages, verbose = FALSE stays silent", {
    dat <- make_test_data()
    expect_message(exclude_qaqc(dat, codes = "CB"), "1 reading")
    expect_message(exclude_qaqc(dat, codes = "NOPE"), "No readings matched")
    expect_silent(exclude_qaqc(dat, codes = "CB", verbose = FALSE))
})
