library(testthat)

# Assumes calc_change_cumu() and example_sets (see helper-example_sets.R)
# are already loaded/sourced into the environment.

# ---- Small synthetic dataset for exact hand-calculated checks -----------
#
# SET 1
#   arm 1, pin 1: heights 10, 12, 15 on d1, d2, d3  -> cumu 0, 2, 5
#   arm 1, pin 2: heights 20, 19, 18 on d1, d2, d3  -> cumu 0, -1, -2
#   arm 2, pin 1: heights 5, 5, 5 on d1, d2, d3     -> cumu 0, 0, 0
#   arm 2, pin 2: heights NA, 3, 4 on d1, d2, d3    -> cumu NA, NA, NA (first reading NA)
#
# SET 2 (kept simple, used mainly to test that SETs don't leak into each other)
#   arm 1, pin 1: heights 100, 101 on d1, d2        -> cumu 0, 1

d1 <- as.Date("2020-01-01")
d2 <- as.Date("2020-06-01")
d3 <- as.Date("2021-01-01")

make_test_data <- function() {
    data.frame(
        set_id       = c(rep("S1", 12), rep("S2", 2)),
        arm_position = c(rep("A1", 6), rep("A2", 6), rep("A1", 2)),
        pin_number   = c(rep(1, 3), rep(2, 3), rep(1, 3), rep(2, 3), rep(1, 2)),
        date         = c(rep(c(d1, d2, d3), 4), d1, d2),
        pin_height   = c(10, 12, 15, 20, 19, 18, 5, 5, 5, NA, 3, 4, 100, 101),
        stringsAsFactors = FALSE
    )
}

# ---- Input validation ---------------------------------------------------

test_that("errors informatively when required columns are missing", {
    bad_dat <- make_test_data() |> dplyr::select(-pin_height)
    expect_error(calc_change_cumu(bad_dat), "must have the following columns")

    bad_dat2 <- make_test_data() |> dplyr::rename(SetID = set_id)
    expect_error(calc_change_cumu(bad_dat2), "must have the following columns")
})

test_that("does not error when all required columns are present, regardless of extra columns", {
    dat <- make_test_data()
    dat$extra_col <- "not used"
    expect_silent(calc_change_cumu(dat))
})

# ---- Structure of the return value --------------------------------------

test_that("returns a list of exactly three named tibbles: pin, arm, set", {
    res <- calc_change_cumu(make_test_data())
    expect_type(res, "list")
    expect_named(res, c("pin", "arm", "set"))
    expect_s3_class(res$pin, "data.frame")
    expect_s3_class(res$arm, "data.frame")
    expect_s3_class(res$set, "data.frame")
})

test_that("pin tibble has one row per input reading, and drops pin_height in favor of cumu", {
    dat <- make_test_data()
    res <- calc_change_cumu(dat)
    expect_equal(nrow(res$pin), nrow(dat))
    expect_true("cumu" %in% names(res$pin))
    expect_false("pin_height" %in% names(res$pin))
})

# ---- Pin-level calculations -----------------------------------------------

test_that("pin cumu is 0 on the first date for each set-arm-pin, except when the first reading itself was NA", {
    res <- calc_change_cumu(make_test_data())
    first_readings <- res$pin |>
        dplyr::group_by(set_id, arm_position, pin_number) |>
        dplyr::filter(date == min(date)) |>
        dplyr::ungroup()

    # every pin whose very first reading was a real number should show cumu == 0
    non_na <- first_readings |> dplyr::filter(!is.na(cumu))
    expect_true(all(non_na$cumu == 0))

    # the only pin allowed to have an NA cumu on its first date is the one
    # whose first pin_height reading was itself NA (S1 / A2 / pin 2)
    na_rows <- first_readings |> dplyr::filter(is.na(cumu))
    expect_equal(nrow(na_rows), 1)
    expect_equal(na_rows$set_id, "S1")
    expect_equal(na_rows$arm_position, "A2")
    expect_equal(na_rows$pin_number, 2)
})

test_that("pin cumu equals the difference from the first reading at that set-arm-pin", {
    res <- calc_change_cumu(make_test_data())

    a1p1 <- res$pin |>
        dplyr::filter(set_id == "S1", arm_position == "A1", pin_number == 1) |>
        dplyr::arrange(date)
    expect_equal(a1p1$cumu, c(0, 2, 5))

    a1p2 <- res$pin |>
        dplyr::filter(set_id == "S1", arm_position == "A1", pin_number == 2) |>
        dplyr::arrange(date)
    expect_equal(a1p2$cumu, c(0, -1, -2))

    a2p1 <- res$pin |>
        dplyr::filter(set_id == "S1", arm_position == "A2", pin_number == 1) |>
        dplyr::arrange(date)
    expect_equal(a2p1$cumu, c(0, 0, 0))
})

test_that("if the first reading for a pin is NA, all cumu values for that pin are NA", {
    res <- calc_change_cumu(make_test_data())
    a2p2 <- res$pin |>
        dplyr::filter(set_id == "S1", arm_position == "A2", pin_number == 2) |>
        dplyr::arrange(date)
    expect_true(all(is.na(a2p2$cumu)))
    expect_equal(nrow(a2p2), 3)  # a row still exists for every reading date
})

test_that("pin-level results do not depend on the row order of the input", {
    dat <- make_test_data()
    shuffled <- dat[sample(nrow(dat)), ]

    res_ordered <- calc_change_cumu(dat)$pin |> dplyr::arrange(set_id, arm_position, pin_number, date)
    res_shuffled <- calc_change_cumu(shuffled)$pin |> dplyr::arrange(set_id, arm_position, pin_number, date)

    expect_equal(res_ordered, res_shuffled)
})

# ---- Arm-level calculations -----------------------------------------------

test_that("arm tibble has one row per set-arm-date and the expected columns", {
    res <- calc_change_cumu(make_test_data())
    expect_true(all(c("mean_cumu", "sd_cumu", "se_cumu") %in% names(res$arm)))
    expect_false(any(c("pin_number", "cumu") %in% names(res$arm)))

    n_expected <- res$pin |>
        dplyr::distinct(set_id, arm_position, date) |>
        nrow()
    expect_equal(nrow(res$arm), n_expected)
})

test_that("arm-level mean/sd/se are calculated correctly and NAs are excluded", {
    res <- calc_change_cumu(make_test_data())

    a1 <- res$arm |> dplyr::filter(set_id == "S1", arm_position == "A1") |> dplyr::arrange(date)
    expect_equal(a1$mean_cumu, c(0, 0.5, 1.5))
    expect_equal(a1$sd_cumu, c(sd(c(0, 0)), sd(c(2, -1)), sd(c(5, -2))))
    expect_equal(a1$se_cumu, a1$sd_cumu / sqrt(2))

    # arm 2: pin 2 is entirely NA, so arm stats should be based on pin 1 alone
    a2 <- res$arm |> dplyr::filter(set_id == "S1", arm_position == "A2") |> dplyr::arrange(date)
    expect_equal(a2$mean_cumu, c(0, 0, 0))
    expect_true(all(is.na(a2$sd_cumu)))   # sd of a single non-NA value is NA
    expect_true(all(is.na(a2$se_cumu)))
})

# ---- Set-level calculations -----------------------------------------------

test_that("set tibble has one row per set-date and the expected columns", {
    res <- calc_change_cumu(make_test_data())
    expect_true(all(c("mean_cumu", "sd_cumu", "se_cumu") %in% names(res$set)))
    expect_false(any(c("arm_position", "mean_value") %in% names(res$set)))

    n_expected <- res$arm |>
        dplyr::distinct(set_id, date) |>
        nrow()
    expect_equal(nrow(res$set), n_expected)
})

test_that("set-level mean/sd/se are calculated correctly from arm-level means", {
    res <- calc_change_cumu(make_test_data())

    s1 <- res$set |> dplyr::filter(set_id == "S1") |> dplyr::arrange(date)
    # arm means per date were: A1 = 0, 0.5, 1.5 and A2 = 0, 0, 0
    expect_equal(s1$mean_cumu, c(0, 0.25, 0.75))
    expect_equal(s1$sd_cumu, c(sd(c(0, 0)), sd(c(0.5, 0)), sd(c(1.5, 0))))
    expect_equal(s1$se_cumu, s1$sd_cumu / sqrt(2))
})

test_that("results for one SET are unaffected by the presence of other SETs", {
    dat <- make_test_data()
    res_full <- calc_change_cumu(dat)

    dat_s1_only <- dat |> dplyr::filter(set_id == "S1")
    res_s1_only <- calc_change_cumu(dat_s1_only)

    expect_equal(
        res_full$pin |> dplyr::filter(set_id == "S1") |> dplyr::arrange(arm_position, pin_number, date),
        res_s1_only$pin |> dplyr::arrange(arm_position, pin_number, date)
    )
    expect_equal(
        res_full$arm |> dplyr::filter(set_id == "S1") |> dplyr::arrange(arm_position, date),
        res_s1_only$arm |> dplyr::arrange(arm_position, date)
    )
    expect_equal(
        res_full$set |> dplyr::filter(set_id == "S1") |> dplyr::arrange(date),
        res_s1_only$set |> dplyr::arrange(date)
    )
})

test_that("a SET with a single arm/pin still produces sensible (NA sd/se) results", {
    res <- calc_change_cumu(make_test_data())
    s2 <- res$set |> dplyr::filter(set_id == "S2") |> dplyr::arrange(date)
    expect_equal(s2$mean_cumu, c(0, 1))
    expect_true(all(is.na(s2$sd_cumu)))
    expect_true(all(is.na(s2$se_cumu)))
})

# ---- Checks against the package's real example_sets dataset --------------

test_that("runs without error on example_sets and preserves row counts", {
    res <- calc_change_cumu(example_sets)
    expect_equal(nrow(res$pin), nrow(example_sets))

    n_arm_expected <- example_sets |> dplyr::distinct(set_id, arm_position, date) |> nrow()
    expect_equal(nrow(res$arm), n_arm_expected)

    n_set_expected <- example_sets |> dplyr::distinct(set_id, date) |> nrow()
    expect_equal(nrow(res$set), n_set_expected)
})

test_that("example_sets: pin-level cumu is correct for SET1, arm a", {
    res <- calc_change_cumu(example_sets)

    pin1 <- res$pin |>
        dplyr::filter(set_id == "SET1", arm_position == "a", pin_number == "pin_1") |>
        dplyr::arrange(date)
    expect_equal(pin1$cumu, c(0, 5, 12))  # heights 100, 105, 112

    pin2 <- res$pin |>
        dplyr::filter(set_id == "SET1", arm_position == "a", pin_number == "pin_2") |>
        dplyr::arrange(date)
    expect_equal(pin2$cumu, c(0, 7, 14))  # heights 106, 113, 120
})

test_that("example_sets: SET1 and SET2 give identical cumu/arm/set results (same pattern, offset heights)", {
    res <- calc_change_cumu(example_sets)

    pin_1 <- res$pin |> dplyr::filter(set_id == "SET1") |> dplyr::arrange(arm_position, pin_number, date) |> dplyr::pull(cumu)
    pin_2 <- res$pin |> dplyr::filter(set_id == "SET2") |> dplyr::arrange(arm_position, pin_number, date) |> dplyr::pull(cumu)
    expect_equal(pin_1, pin_2)

    arm_1 <- res$arm |> dplyr::filter(set_id == "SET1") |> dplyr::arrange(arm_position, date) |> dplyr::select(-set_id)
    arm_2 <- res$arm |> dplyr::filter(set_id == "SET2") |> dplyr::arrange(arm_position, date) |> dplyr::select(-set_id)
    expect_equal(arm_1, arm_2)

    set_1 <- res$set |> dplyr::filter(set_id == "SET1") |> dplyr::arrange(date) |> dplyr::select(-set_id)
    set_2 <- res$set |> dplyr::filter(set_id == "SET2") |> dplyr::arrange(date) |> dplyr::select(-set_id)
    expect_equal(set_1, set_2)
})

test_that("example_sets: arm- and SET-level means/sd/se match hand calculations for SET1", {
    res <- calc_change_cumu(example_sets)

    arm_a <- res$arm |> dplyr::filter(set_id == "SET1", arm_position == "a") |> dplyr::arrange(date)
    # cumu per pin per date -> pin_1: 0,5,12 ; pin_2: 0,7,14 ; pin_3: 0,2,6
    expect_equal(arm_a$mean_cumu, c(0, mean(c(5, 7, 2)), mean(c(12, 14, 6))))
    expect_equal(arm_a$sd_cumu, c(sd(c(0, 0, 0)), sd(c(5, 7, 2)), sd(c(12, 14, 6))))
    expect_equal(arm_a$se_cumu, arm_a$sd_cumu / sqrt(3))

    arm_b <- res$arm |> dplyr::filter(set_id == "SET1", arm_position == "b") |> dplyr::arrange(date)
    # pin_1: 0,7,15 ; pin_2: 0,3,-2 ; pin_3: 0,3,15
    expect_equal(arm_b$mean_cumu, c(0, mean(c(7, 3, 3)), mean(c(15, -2, 15))))

    set1 <- res$set |> dplyr::filter(set_id == "SET1") |> dplyr::arrange(date)
    expect_equal(
        set1$mean_cumu,
        c(0, mean(c(arm_a$mean_cumu[2], arm_b$mean_cumu[2])), mean(c(arm_a$mean_cumu[3], arm_b$mean_cumu[3])))
    )
})

# ---- Custom column names (generalization) ---------------------------------

test_that("custom column names produce the same results as the defaults", {
    dat <- make_test_data() |>
        dplyr::rename(reading_date = date, elevation = pin_height)

    res_custom <- calc_change_cumu(dat, date = reading_date, pin_height = elevation)
    res_default <- calc_change_cumu(make_test_data())

    # output column names are always the canonical ones, regardless of input names
    expect_true(all(c("date", "set_id", "arm_position", "pin_number", "cumu") %in% names(res_custom$pin)))

    expect_equal(res_custom$pin$cumu, res_default$pin$cumu)
    expect_equal(res_custom$arm, res_default$arm)
    expect_equal(res_custom$set, res_default$set)
})

test_that("missing-column error message reflects the custom name that was requested", {
    dat <- make_test_data() |> dplyr::rename(elevation = pin_height)
    expect_error(calc_change_cumu(dat, pin_height = elevation_mm), "elevation_mm")
})
