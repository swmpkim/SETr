#' Import and combine NERR SWMP wide-format data from an Excel workbook
#'
#' @description
#' Reads one or more sheets from a NERR-formatted Excel file, row-binds them
#' into a single data frame, and verifies that the `set_id` column in each
#' sheet matches the name of the sheet it came from. If any mismatches are
#' found, the function stops and prints the offending rows for review.
#'
#' TODO: expand on what "NERR wide" data represents, and any assumptions
#' about file/sheet structure (e.g. required columns, expected sheet naming
#' convention like "SITE-#").
#'
#' @param file Character. Path to the Excel file to read.
#' @param sheets Character vector of sheet names to read, or `"all"` to read
#'   every sheet in the file. Defaults to `"all"`. Use this to exclude sheets
#'   that shouldn't be imported (e.g. a README or metadata sheet).
#'
#' @return A data frame combining all specified sheets, with `set_id`,
#'   `arm_position`, and any pin QAQC column (matching `pin_<n>_qaqc`, the
#'   current convention, or the legacy `pin_<n>_qaqc_code`) coerced to
#'   character, and any pin height column (matching `pin_<n>_height_<unit>`)
#'   coerced to numeric. The temporary `sheet` column (used to identify sheet
#'   origin during the mismatch check) is dropped from the final output. If
#'   `year`, `month`, and `day` columns are all present and there is not
#'   already a `date` column, a `date` column is added by combining them.
#'
#' @details
#' TODO: note any expectations about column names/types across sheets
#' (e.g. `set_id`, `year`, `month`, `arm_position` must exist in every sheet),
#' and what happens if sheets have inconsistent columns.
#'
#' @section Errors:
#' Stops execution if any row's `set_id` does not match its source sheet
#' name, printing the mismatched rows (`sheet`, `set_id`, `year`, `month`,
#' `arm_position`) for inspection.
#'
#' @examples
#' wide_file <- system.file("extdata", "example_wide.xlsx", package = "SETr")
#'
#' # Read all sheets
#' dat <- import_NERRwide(wide_file)
#'
#' # Read only specific sheets
#' dat <- import_NERRwide(wide_file, sheets = c("CLMAJ-1", "SPALT-1"))
#'
#' @export

import_NERRwide <- function(file,
                            sheets = "all"){
    # sheets lets you choose what to read in - e.g. if there's a readme sheet,
    # you could *not* include it in the sheets to be read

    shts <- if (identical(sheets, "all")) {
        readxl::excel_sheets(file)
    } else {
        sheets
    }

    dat <- shts |>
        purrr::set_names() |>
        purrr::map(~ readxl::read_excel(path = file,
                                        sheet = .x,
                                        col_types = "text",
                                        na = c("", "NA"))) |>
        purrr::list_rbind(names_to = "sheet")

    # check to make sure the SET ID that was entered matches the name of each sheet
    mismatches <- dat$set_id != dat$sheet

    # make this whole script stop if something doesn't match
    if(sum(mismatches) > 0){
        print(dat[mismatches, c("sheet", "set_id", "year", "month", "arm_position")])
        stop("There are SET IDs that do not match the sheet name. Please check and correct the rows printed above before proceeding.")
    }else{
        # if no problem, do everything else

        # first, format the data:
        # get rid of the "sheet" column
        # several columns should be character: set_id, arm_position,
        # and any pin QAQC column (pin_<n>_qaqc or legacy pin_<n>_qaqc_code)
        dat_formatted <- dat |>
            dplyr::select(-"sheet") |>
            dplyr::mutate(dplyr::across(c("set_id",
                                          "arm_position",
                                          tidyselect::matches("_qaqc(_code)?$")),
                                        as.character)) |>
            dplyr::mutate(dplyr::across(tidyselect::matches("^pin_\\d+_height"), as.numeric))

        # combine year/month/day into a proper date, if those columns exist
        # and there isn't already a date column
        if (all(c("year", "month", "day") %in% names(dat_formatted)) &&
            !"date" %in% names(dat_formatted)) {
            dat_formatted <- dat_formatted |>
                dplyr::mutate(date = as.Date(paste(.data$year, .data$month, .data$day, sep = "-"))) |>
                dplyr::relocate("date", .before = "year")
        }

    }

        return(dat_formatted)
}
