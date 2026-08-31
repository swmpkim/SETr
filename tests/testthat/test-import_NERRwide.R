library(testthat)

skip_if_not_installed("writexl")

# ---- helper: build a small one-sheet-worth-of-data data frame --------------

make_sheet_df <- function(set_id, qaqc_suffix = c("qaqc", "qaqc_code")) {
    qaqc_suffix <- match.arg(qaqc_suffix)

    df <- data.frame(
        set_id          = set_id,
        year            = 2020,
        month           = 1,
        day             = 15,
        arm_position    = c("a", "b"),
        pin_1_height_mm = c(100, 110),
        stringsAsFactors = FALSE
    )
    df[[paste0("pin_1_", qaqc_suffix)]] <- c("Y", "N")
    df
}

write_temp_workbook <- function(sheets) {
    tmp <- tempfile(fileext = ".xlsx")
    writexl::write_xlsx(sheets, path = tmp)
    tmp
}

# ---- QAQC suffix handling ---------------------------------------------------

test_that("imports a workbook using the current QAQC suffix (pin_<n>_qaqc)", {
    tmp <- write_temp_workbook(list(SET1 = make_sheet_df("SET1", "qaqc")))
    on.exit(unlink(tmp), add = TRUE)

    result <- import_NERRwide(tmp)

    expect_equal(nrow(result), 2)
    expect_false("sheet" %in% names(result))
    expect_true(is.character(result$set_id))
    expect_true(is.character(result$arm_position))
    expect_true(is.character(result$pin_1_qaqc))
})

# ---- year/month/day -> date ------------------------------------------------

test_that("combines year/month/day into a date column", {
    tmp <- write_temp_workbook(list(SET1 = make_sheet_df("SET1", "qaqc")))
    on.exit(unlink(tmp), add = TRUE)

    result <- import_NERRwide(tmp)

    expect_true("date" %in% names(result))
    expect_true(all(result$date == as.Date("2020-01-15")))
    # year/month/day are left in place, not dropped
    expect_true(all(c("year", "month", "day") %in% names(result)))
})

test_that("does not add a date column when year/month/day aren't all present", {
    tmp <- tempfile(fileext = ".xlsx")
    on.exit(unlink(tmp), add = TRUE)

    no_day <- make_sheet_df("SET1", "qaqc")
    no_day$day <- NULL
    writexl::write_xlsx(list(SET1 = no_day), path = tmp)

    result <- import_NERRwide(tmp)

    expect_false("date" %in% names(result))
})

test_that("imports a workbook using the legacy QAQC suffix (pin_<n>_qaqc_code)", {
    tmp <- write_temp_workbook(list(SET1 = make_sheet_df("SET1", "qaqc_code")))
    on.exit(unlink(tmp), add = TRUE)

    result <- import_NERRwide(tmp)

    expect_equal(nrow(result), 2)
    expect_true(is.character(result$pin_1_qaqc_code))
})

# ---- Sheet/set_id mismatch check -------------------------------------------

test_that("errors when a sheet's set_id values don't match its sheet name", {
    tmp <- write_temp_workbook(list(SET1 = make_sheet_df("WRONG_ID", "qaqc")))
    on.exit(unlink(tmp), add = TRUE)

    expect_error(import_NERRwide(tmp), "do not match the sheet name")
})

# ---- End-to-end with pivot_NERRwide() ---------------------------------------

test_that("import then pivot works end-to-end for both QAQC conventions", {
    tmp_new <- write_temp_workbook(list(SET1 = make_sheet_df("SET1", "qaqc")))
    tmp_old <- write_temp_workbook(list(SET1 = make_sheet_df("SET1", "qaqc_code")))
    on.exit(unlink(c(tmp_new, tmp_old)), add = TRUE)

    piv_new <- import_NERRwide(tmp_new) |> pivot_NERRwide()
    piv_old <- import_NERRwide(tmp_old) |> pivot_NERRwide()

    expect_true(all(c("height_mm", "qaqc_code") %in% names(piv_new)))
    expect_true(all(c("height_mm", "qaqc_code") %in% names(piv_old)))
    expect_equal(piv_new$qaqc_code, piv_old$qaqc_code)
})
