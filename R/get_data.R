#' Collect data from three spreadsheets. User must assign name of data frame generated.
#' All functions default to look for data as `bs_df`
#'
#' This function collects data from three different sources.
#' All data is read-in directly from the .xlsx files. Director and file names must be entered.
#'
#' The following data treatment is performed:
#'
#' The error is divided by 2 so that the package will work with single coverage uncertainty terms.
#'
#' Unused data columns are removed. Lab data (`lab_results` and `received_dates`) are combined.
#' Then the data is `left_join`ed by SAMPLE_ID to `spikeval` (retaining all rows in `spikeval`).
#'
#' Dates are converted to date formats. Isotopes are changed to proper capitalization.
#'
#' False negative and false positive values are flagged with an added `err_type` column.
#'
#' @param my_dir directory where data files are stored in format 'C:/my_data_loc/'.
#' Note the forward slashes.
#'
#' @param spikeval name of the file containing the spike values.
#' This file must include the following in columns A-E: SAMPLE_ID, Received, Result,
#' ISOTOPE, and SPIKED_VALUE. Default file name is 'Spiked Values.xlsx'
#'
#' @param lab_results name of the file containing the spike values. This file must include
#' the following columns in the range of A-I: SAMPLE_ID, RESULT_DATE, ISOTOPE, ACTIVITY,
#' ERROR, and DET_LEVEL. No default.
#'
#' @param received_dates name of the file containing the lab's received and posted dates. This
#' file must include the following columns in the range of A-C: lngSampleNumber,
#' dtmDateLogged, and dtmReceiveDate. Default file name = 'EBL Received Dates.xlsx'
#'
#' @return data frame containing all needed data to be used in subsequent functions.
#' # add example
#' @export

get_data <- function(my_dir, lab_results, spikeval = "Spiked Values.xlsx", received_dates = "EBL Received Dates.xlsx") {

    setwd(my_dir)
    spike_df <- readxl::read_excel(path = paste0(my_dir, "/", spikeval), range = readxl::cell_cols("A:E"))
    res_df <- readxl::read_excel(path = paste0(my_dir, "/", lab_results), range = readxl::cell_cols("A:I"))
    rec_df <- readxl::read_excel(path = paste0(my_dir, "/", received_dates), range = readxl::cell_cols("A:C"))

    # retain only columns called for
    spike_df <- spike_df %>%
        dplyr::select(c("SAMPLE_ID", "Received", "Result", "ISOTOPE", "SPIKED_VALUE"))
    res_df <- res_df %>%
        dplyr::select(c("SAMPLE_ID", "RESULT_DATE", "ISOTOPE", "ACTIVITY", "ERROR",
            "DET_LEVEL"))
    rec_df <- rec_df %>%
        dplyr::select(c("lngSampleNumber", "dtmDateLogged", "dtmReceiveDate"))

    bs_df <- dplyr::left_join(res_df, rec_df, by = c(SAMPLE_ID = "lngSampleNumber")) %>%
        dplyr::left_join(spike_df)

    # correct data structure
    bs_df$SAMPLE_ID <- as.factor(bs_df$SAMPLE_ID)
    bs_df$RESULT_DATE <- as.Date(substr(bs_df$RESULT_DATE, 1, 10), format = "%Y-%m-%d")

    # make these itotopes look a little better
    bs_df$ISOTOPE[bs_df$ISOTOPE == "AM-241"] <- "Am-241"
    bs_df$ISOTOPE[bs_df$ISOTOPE == "NP-237"] <- "Np-237"
    bs_df$ISOTOPE[bs_df$ISOTOPE == "PU-238"] <- "Pu-238"
    bs_df$ISOTOPE[bs_df$ISOTOPE == "PU-239"] <- "Pu-239"

    # data cleanup
    bs_df$ISOTOPE <- as.factor(bs_df$ISOTOPE)
    bs_df$dtmDateLogged <- as.Date(bs_df$dtmDateLogged)
    bs_df$dtmReceiveDate <- as.Date(bs_df$dtmReceiveDate)
    bs_df$Received <- as.Date(bs_df$Received)
    bs_df$Result <- as.Date(bs_df$Result)
    bs_df$dtmDateLogged <- as.Date(bs_df$dtmDateLogged)
    bs_df$SPIKED_VALUE <- as.numeric(bs_df$SPIKED_VALUE)

    # Spiked Values that are NA are blanks, so no spike value, therefore set to
    # zero
    bs_df$SPIKED_VALUE[is.na(bs_df$SPIKED_VALUE)] <- 0

    # add column for activity - detection level where negative = not detected
    bs_df$del_act_det <- bs_df$ACTIVITY - bs_df$DET_LEVEL

    # identify false negatives and false positives
    bs_df <- bs_df %>%
        dplyr::mutate(err_type = dplyr::case_when(del_act_det > 0 & SPIKED_VALUE ==
            0 ~ "false_pos", del_act_det < 0 & SPIKED_VALUE > 0 ~ "false_neg", TRUE ~
            "no_err_cat"))

    # make the error k=1, expecting lab report at k=2
    bs_df$ERROR <- bs_df$ERROR/2


    readr::write_tsv(bs_df, file = paste0(my_dir, "/", "bs_df.tsv"))
    bs_df
}
