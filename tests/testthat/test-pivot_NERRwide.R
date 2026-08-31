library(testthat)

# ---- helper: build a small two-pin wide data frame ------------------------

make_wide <- function(qaqc_suffix = c("qaqc", "qaqc_code"), height_unit = "mm") {
    qaqc_suffix <- match.arg(qaqc_suffix)

    df <- data.frame(
        set_id       = c("SET1", "SET1"),
        arm_position = c("a", "b"),
        date         = as.Date(c("2020-01-01", "2020-01-01")),
        stringsAsFactors = FALSE
    )

    df[[paste0("pin_1_height_", height_unit)]] <- c(100, 110)
    df[[paste0("pin_1_", qaqc_suffix)]]        <- c("Y", "N")
    df[[paste0("pin_2_height_", height_unit)]] <- c(105, 112)
    df[[paste0("pin_2_", qaqc_suffix)]]        <- c("N", "Y")

    df
}

# ---- QAQC suffix handling ---------------------------------------------------

test_that("pivots the current QAQC suffix (pin_<n>_qaqc)", {
    result <- pivot_NERRwide(make_wide(qaqc_suffix = "qaqc"))

    expect_true(all(c("pin_number", "height_mm", "qaqc_code") %in% names(result)))
    expect_equal(nrow(result), 4)  # 2 original rows x 2 pins

    row1 <- result[result$arm_position == "a" & result$pin_number == "1", ]
    expect_equal(row1$height_mm, 100)
    expect_equal(row1$qaqc_code, "Y")
})

test_that("pivots the legacy QAQC suffix (pin_<n>_qaqc_code)", {
    result <- pivot_NERRwide(make_wide(qaqc_suffix = "qaqc_code"))

    expect_true(all(c("pin_number", "height_mm", "qaqc_code") %in% names(result)))
    expect_equal(nrow(result), 4)

    row1 <- result[result$arm_position == "a" & result$pin_number == "1", ]
    expect_equal(row1$height_mm, 100)
    expect_equal(row1$qaqc_code, "Y")
})

test_that("current and legacy QAQC suffixes produce identical results", {
    current <- pivot_NERRwide(make_wide(qaqc_suffix = "qaqc")) |>
        dplyr::arrange(arm_position, pin_number)
    legacy <- pivot_NERRwide(make_wide(qaqc_suffix = "qaqc_code")) |>
        dplyr::arrange(arm_position, pin_number)

    expect_equal(current, legacy)
})

# ---- Height unit handling (already generic; guarding against regressions) --

test_that("legacy height unit (cm) still pivots into a height_cm column", {
    result <- pivot_NERRwide(make_wide(qaqc_suffix = "qaqc", height_unit = "cm"))

    expect_true("height_cm" %in% names(result))
    expect_false("height_mm" %in% names(result))
})

test_that("current height unit (mm) pivots into a height_mm column", {
    result <- pivot_NERRwide(make_wide(qaqc_suffix = "qaqc", height_unit = "mm"))

    expect_true("height_mm" %in% names(result))
    expect_false("height_cm" %in% names(result))
})

# ---- Input validation --------------------------------------------------------

test_that("errors informatively when there are no pin height columns", {
    dat <- data.frame(set_id = "SET1", arm_position = "a", pin_1_qaqc = "Y")
    expect_error(pivot_NERRwide(dat), "pin height columns")
})
