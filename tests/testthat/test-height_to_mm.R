library(testthat)

test_that("height_cm is converted to mm, renamed to pin_height, and the source column dropped", {
    df <- data.frame(site = c("SET1", "SET2"), height_cm = c(15, 18))
    result <- height_to_mm(df)

    expect_equal(result$pin_height, c(150, 180))
    expect_false("height_cm" %in% names(result))
    expect_true("pin_height" %in% names(result))
})

test_that("height_mm is renamed to pin_height, unchanged, and the source column dropped", {
    df <- data.frame(site = c("SET1", "SET2"), height_mm = c(156, 182))
    result <- height_to_mm(df)

    expect_equal(result$pin_height, c(156, 182))
    expect_false("height_mm" %in% names(result))
})

test_that("other columns are left untouched", {
    df <- data.frame(site = c("SET1", "SET2"), date = as.Date(c("2020-01-01", "2020-06-01")),
                      height_mm = c(100, 110), stringsAsFactors = FALSE)
    result <- height_to_mm(df)

    expect_equal(result$site, df$site)
    expect_equal(result$date, df$date)
})

test_that("data with neither height_cm nor height_mm is returned unchanged", {
    df <- data.frame(site = c("SET1", "SET2"), pin_height = c(100, 110))
    result <- height_to_mm(df)

    expect_equal(result, df)
    expect_false("height_cm" %in% names(result))
    expect_false("height_mm" %in% names(result))
})

test_that("if both height_cm and height_mm are present, height_mm wins (documents current behavior)", {
    # height_cm is converted+dropped first, then the height_mm check overwrites
    # pin_height with the raw height_mm value and drops height_mm -- so the
    # height_cm-derived value never survives. This test locks in that behavior
    # so a change to it is a deliberate decision, not an accident.
    df <- data.frame(site = "SET1", height_cm = 15, height_mm = 999)
    result <- height_to_mm(df)

    expect_equal(result$pin_height, 999)
    expect_false("height_cm" %in% names(result))
    expect_false("height_mm" %in% names(result))
})
