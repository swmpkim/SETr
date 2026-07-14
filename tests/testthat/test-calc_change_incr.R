## Unit tests for calc_change_incr()
## Place this file at tests/testthat/test-calc_change_incr.R in your package.
## Requires: testthat (>= 3rd edition style used below, but works with 2nd edition too),
##           tibble, dplyr

library(testthat)



# =============================================================================
# 1. Input validation
# =============================================================================

test_that("errors informatively when required columns are missing", {

    expect_error(
        calc_change_incr(example_sets[, -which(names(example_sets) == "set_id")]),
        "set_id"
    )

    expect_error(
        calc_change_incr(example_sets[, -which(names(example_sets) == "pin_height")]),
        "pin_height"
    )

    # dropping several columns at once - message should still fire and mention
    # at least one of the missing columns
    expect_error(
        calc_change_incr(example_sets[, c("date", "set_id")]),
        "arm_position"
    )
})

test_that("does not error when all required columns are present, regardless of column order", {

    reordered <- example_sets[, c("pin_height", "date", "pin_number",
                                  "arm_position", "set_id")]
    expect_silent(calc_change_incr(reordered))
})

test_that("does not error when extra, non-required columns are present", {

    extra <- example_sets
    extra$notes <- "some qc comment"
    expect_silent(calc_change_incr(extra))
})

# =============================================================================
# 2. Overall structure of the return value
# =============================================================================

test_that("returns a list of exactly three tibbles named pin, arm, set", {

    result <- calc_change_incr(example_sets)

    expect_type(result, "list")
    expect_named(result, c("pin", "arm", "set"))
    expect_s3_class(result$pin, "tbl_df")
    expect_s3_class(result$arm, "tbl_df")
    expect_s3_class(result$set, "tbl_df")
})

test_that("pin tibble has one row per input reading and expected columns", {

    result <- calc_change_incr(example_sets)

    expect_equal(nrow(result$pin), nrow(example_sets))
    expect_true(all(c("date", "set_id", "arm_position", "pin_number",
                      "pin_height", "incr") %in% names(result$pin)))
})

test_that("arm tibble has one row per set-arm-date combination", {

    result <- calc_change_incr(example_sets)

    expected_rows <- example_sets %>%
        dplyr::distinct(set_id, arm_position, date) %>%
        nrow()

    expect_equal(nrow(result$arm), expected_rows)
    expect_true(all(c("set_id", "arm_position", "date", "mean_incr",
                      "sd_incr", "se_incr") %in% names(result$arm)))
    expect_false("pin_number" %in% names(result$arm))
})

test_that("set tibble has one row per set-date combination", {

    result <- calc_change_incr(example_sets)

    expected_rows <- example_sets %>%
        dplyr::distinct(set_id, date) %>%
        nrow()

    expect_equal(nrow(result$set), expected_rows)
    expect_true(all(c("set_id", "date", "mean_incr", "sd_incr",
                      "se_incr") %in% names(result$set)))
    expect_false("arm_position" %in% names(result$set))
})

# =============================================================================
# 3. Pin-level calculations against known values from example_sets
# =============================================================================

test_that("pin-level incr is NA on the first date for every pin", {

    result <- calc_change_incr(example_sets)

    first_date_rows <- result$pin %>%
        dplyr::group_by(set_id, arm_position, pin_number) %>%
        dplyr::filter(date == min(date)) %>%
        dplyr::ungroup()

    expect_true(all(is.na(first_date_rows$incr)))
})

test_that("pin-level incr matches hand-calculated differences (SET1, arm a)", {

    result <- calc_change_incr(example_sets)

    pin1 <- result$pin %>%
        dplyr::filter(set_id == "SET1", arm_position == "a", pin_number == "pin_1") %>%
        dplyr::arrange(date)

    expect_equal(pin1$pin_height, c(100, 105, 112))
    expect_equal(pin1$incr, c(NA, 5, 7))

    pin2 <- result$pin %>%
        dplyr::filter(set_id == "SET1", arm_position == "a", pin_number == "pin_2") %>%
        dplyr::arrange(date)

    expect_equal(pin2$incr, c(NA, 7, 7))

    pin3 <- result$pin %>%
        dplyr::filter(set_id == "SET1", arm_position == "a", pin_number == "pin_3") %>%
        dplyr::arrange(date)

    expect_equal(pin3$incr, c(NA, 2, 4))
})

test_that("pin-level calculations do not mix across set_id, arm_position, or pin_number", {

    # SET1 arm b, pin_1 has a completely different trajectory than SET1 arm a, pin_1
    result <- calc_change_incr(example_sets)

    set1_a_pin1 <- result$pin %>%
        dplyr::filter(set_id == "SET1", arm_position == "a", pin_number == "pin_1") %>%
        dplyr::arrange(date) %>% dplyr::pull(incr)

    set1_b_pin1 <- result$pin %>%
        dplyr::filter(set_id == "SET1", arm_position == "b", pin_number == "pin_1") %>%
        dplyr::arrange(date) %>% dplyr::pull(incr)

    expect_false(isTRUE(all.equal(set1_a_pin1, set1_b_pin1)))
})

# =============================================================================
# 4. Arm- and set-level calculations against known values from example_sets
# =============================================================================

test_that("arm-level mean/sd/se match hand-calculated values (SET1, arm a)", {

    result <- calc_change_incr(example_sets)

    arm_a <- result$arm %>%
        dplyr::filter(set_id == "SET1", arm_position == "a") %>%
        dplyr::arrange(date)

    # first date: all pin increments are NA -> mean is NaN, sd/se are NA
    expect_true(is.nan(arm_a$mean_incr[1]))
    expect_true(is.na(arm_a$sd_incr[1]))
    expect_true(is.na(arm_a$se_incr[1]))

    # second date: incr values are 5, 7, 2 -> mean = 4.6667, sd = 2.5166
    expect_equal(arm_a$mean_incr[2], mean(c(5, 7, 2)), tolerance = 1e-4)
    expect_equal(arm_a$sd_incr[2], stats::sd(c(5, 7, 2)), tolerance = 1e-4)
    expect_equal(arm_a$se_incr[2],
                 stats::sd(c(5, 7, 2)) / sqrt(3), tolerance = 1e-4)

    # third date: incr values are 7, 7, 4 -> mean = 6, sd = 1.7321
    expect_equal(arm_a$mean_incr[3], mean(c(7, 7, 4)), tolerance = 1e-4)
    expect_equal(arm_a$sd_incr[3], stats::sd(c(7, 7, 4)), tolerance = 1e-4)
})

test_that("set-level mean/sd/se match hand-calculated values (SET1)", {

    result <- calc_change_incr(example_sets)

    set1 <- result$set %>%
        dplyr::filter(set_id == "SET1") %>%
        dplyr::arrange(date)

    expect_true(is.nan(set1$mean_incr[1]))
    expect_true(is.na(set1$sd_incr[1]))

    # arm-level means feeding into the set level on date 2 are 4.6667 (arm a)
    # and 4.3333 (arm b) -> set mean = 4.5
    expect_equal(set1$mean_incr[2], 4.5, tolerance = 1e-3)

    # date 3: arm means are 6 (arm a) and 5 (arm b) -> set mean = 5.5
    expect_equal(set1$mean_incr[3], 5.5, tolerance = 1e-3)
})

test_that("set_id groups are aggregated independently at the arm and set level", {

    result <- calc_change_incr(example_sets)

    set1_arm <- result$arm %>% dplyr::filter(set_id == "SET1") %>%
        dplyr::arrange(arm_position, date) %>% dplyr::select(-set_id)
    set2_arm <- result$arm %>% dplyr::filter(set_id == "SET2") %>%
        dplyr::arrange(arm_position, date) %>% dplyr::select(-set_id)

    # By construction, example_sets has SET2 = SET1 + 42 at every pin, so
    # increments (and therefore arm/set summaries) should be identical even
    # though the raw pin heights are not.
    expect_equal(set1_arm, set2_arm)

    raw1 <- example_sets %>% dplyr::filter(set_id == "SET1") %>%
        dplyr::arrange(arm_position, pin_number, date) %>% dplyr::pull(pin_height)
    raw2 <- example_sets %>% dplyr::filter(set_id == "SET2") %>%
        dplyr::arrange(arm_position, pin_number, date) %>% dplyr::pull(pin_height)

    expect_false(isTRUE(all.equal(raw1, raw2)))
})

# =============================================================================
# 5. Small, hand-built dataset exercising edge cases directly
# =============================================================================

test_that("arm- and set-level stats match by-hand calculations on a minimal dataset", {

    toy <- data.frame(
        date         = as.Date(rep(c("2020-01-01", "2020-06-01", "2021-01-01"), times = 3)),
        set_id       = "S1",
        arm_position = c(rep("x", 6), rep("y", 3)),
        pin_number   = c(rep("p1", 3), rep("p2", 3), rep("p1", 3)),
        pin_height   = c(10, 15, 25,     # arm x, pin 1 -> incr NA, 5, 10
                         20, 20, 26,     # arm x, pin 2 -> incr NA, 0, 6
                         100, 102, 104)  # arm y, pin 1 -> incr NA, 2, 2
    )

    result <- calc_change_incr(toy)

    arm_x <- result$arm %>% dplyr::filter(arm_position == "x") %>% dplyr::arrange(date)
    expect_equal(arm_x$mean_incr, c(NaN, 2.5, 8))
    expect_equal(arm_x$sd_incr[2], stats::sd(c(5, 0)), tolerance = 1e-8)
    expect_equal(arm_x$sd_incr[3], stats::sd(c(10, 6)), tolerance = 1e-8)

    arm_y <- result$arm %>% dplyr::filter(arm_position == "y") %>% dplyr::arrange(date)
    expect_equal(arm_y$mean_incr, c(NaN, 2, 2))
    # only one pin in arm y -> sd of a single value is NA
    expect_true(all(is.na(arm_y$sd_incr[2:3])))

    set_lvl <- result$set %>% dplyr::arrange(date)
    # date 2: arm means feeding in are 2.5 (x) and 2 (y) -> set mean = 2.25
    expect_equal(set_lvl$mean_incr[2], 2.25, tolerance = 1e-8)
    # date 3: arm means feeding in are 8 (x) and 2 (y) -> set mean = 5
    expect_equal(set_lvl$mean_incr[3], 5, tolerance = 1e-8)
})

test_that("a single reading date per pin produces all-NA increments and NaN/NA summaries", {

    one_date <- data.frame(
        date         = as.Date("2020-01-01"),
        set_id       = c("S1", "S1"),
        arm_position = c("x", "x"),
        pin_number   = c("p1", "p2"),
        pin_height   = c(10, 12)
    )

    result <- calc_change_incr(one_date)

    expect_true(all(is.na(result$pin$incr)))
    expect_true(is.nan(result$arm$mean_incr))
    expect_true(is.na(result$arm$sd_incr))
    expect_true(is.na(result$arm$se_incr))
    expect_true(is.nan(result$set$mean_incr))
})


