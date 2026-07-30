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
#'   `arm_position`, and any column ending in `qaqc_code` coerced to
#'   character. The temporary `sheet` column (used to identify sheet origin
#'   during the mismatch check) is dropped from the final output.
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
#' \dontrun{
#' # Read all sheets
#' dat <- import_NERRwide("path/to/file.xlsx")
#'
#' # Read only specific sheets
#' dat <- import_NERRwide("path/to/file.xlsx", sheets = c("CLMAJ-1", "CLMAJ-2"))
#' }
#'
#' @export

import_NERRwide <- function(file,
                            sheets = "all"){
    # sheets lets you choose what to read in - e.g. if there's a readme sheet,
    # you could *not* include it in the sheets to be read

    shts <- if (sheets == "all") {
        readxl::excel_sheets(file)
    } else {
        sheets
    }

    dat <- shts %>%
        purrr::set_names() %>%
        purrr::map(~ readxl::read_excel(path = file,
                                        sheet = .x,
                                        col_types = "text",
                                        na = c("", "NA"))) %>%
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
        # and anything that ends in qaqc_code
        dat_formatted <- dat %>%
            dplyr::select(-"sheet") %>%
            dplyr::mutate(dplyr::across(c(.data$set_id,
                                          .data$arm_position,
                                          dplyr::ends_with("qaqc_code")),
                                        as.character))

    }

        return(dat_formatted)
}
