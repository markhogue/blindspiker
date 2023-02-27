#' Load data associated with the spiked samples and laboratory results
#' from comma separated variable text files.
#'
#' This function loads spike data and the laboratory results, then processes
#' it so that the `sample_ID` ties the spike value and the results together,
#' using the `left_join` function from the `dplyr` package.
#'
#' False negative results are flagged for laboratory results below the detection
#' level in the analysis of a sample spiked above the detection level.
#' False positives are flagged for laboratory results above the detection when
#' the analyte was not spiked. Error rates are computed with the `table_false`
#' function.
#'
#' To use this function, first set up spike value data in a .csv file
#' (in any column order) matching the column headers of the following values:
#'
#' Required for spike data:
#' `sample_ID` unique identifier, character or numeric
#' `analyte` character data
#' `spike_value` numeric value
#' `spike_units` character data
#' `submission_date` character data that will be converted to date in format
#'  YYYY-MM-DD (for example 1999-12-31)
#'
#' Optional for spike data:
#' `sv_unc` numeric, the uncertainty of the spike value
#' `sv_k` the coverage factor for the spike value uncertainty (typically 1 or 2)
#' `provider lab` character name of laboratory providing spiked samples
#'
#' Required for laboratory results:
#' `sample_ID` must match spike `sample_ID`
#' `analyte` must match spike `analyte`
#' `result` numeric value
#' `units` must match `spike_units`
#' `result_date`
#' `det_lvl` numeric detection level
#' `unc` numeric uncertainty of the laboratory result
#' `k` the coverage factor for the result uncertainty (typically 1 or 2)
#'
#' Note that the two data sets (spike values and laboratory results) will
#' be combined by `sample_ID`, and also by `analyte` if present in both sets.
#' (If the laboratory data includes a non-zero result for an analyte not
#' present in the spike data, that would indicate a false positive.)
#'
#' Use this function to load the spike data and establish a data frame named
#' `bs_df`. All plotting and data analysis functions will default to look for
#' this data set.
#'
#' @param spike_data name of the file, with path, containing the spike values.
#' Example form, "C:/my_directory/my_spike_data.csv".
#' This file should contain column headings identified above and may contain
#' additional data identified above. If your file has different column names,
#' you will have to identify them in the plotting and data analysis function
#' parameters. This parameter is required, with no default.
#'
#' @param lab_data name of the file, with path, containing the laboratory
#' results. Example form, "C:/my_directory/my_lab_data.csv".
#' #' This file should contain column headings identified above and may contain
#' additional data identified above. If your file has different column names,
#' you will have to identify them in the plotting and data analysis function
#' parameters. This parameter is required, with no default.
#'
#' @return data frame containing all needed data to be used in subsequent
#' functions.
#'
#' @examples
#' example_spike_data <- system.file("extdata", "spikevals.csv", package = "blindspiker")
#' example_lab_data <- system.file("extdata", "labvals.csv", package = "blindspiker")
#' example_df <- get_data(spike_data = example_spike_data, lab_data = example_lab_data)
#'
#' @export

get_data <- function(spike_data,
                           lab_data) {

  # To avoid visible binding note in package check:
  analyte <- units <- spike_unit <- spike_value <- NULL

  cat("Be sure to save the data to the global environment!")
  cat("\n")
  cat("bs_df <- get_spike_data...is recommended")
  cat("\n")
  cat("\n")
      # read spike data
    spike_df <- utils::read.csv(file = spike_data)

    # read lab data
    lab_df <- utils::read.csv(file = lab_data)

    # check for analyte match
    # if not spiked, it probably shouldn't be in lab data
    spike_analytes <- unique(spike_df$analyte)
    lab_analytes <- unique(lab_df$analyte)

    # get lab_analytes index matching spike_analytes
    analyte_mismatch_ind <- !lab_analytes %in% spike_analytes
    analyte_mismatch <- lab_analytes[analyte_mismatch_ind]

    # print mismatched analytes to screen
      if(any(analyte_mismatch_ind == TRUE)) {
        cat(paste0("WARNING: ", analyte_mismatch,
          " is reported by the lab, but is not in spike data. \n")) }
    else{ cat("Analytes check - lab data and spike analytes all match.")
    }
      cat("\n")
      cat("\n")

    bs_df <-
        dplyr::full_join(spike_df,lab_df,
                          by = c("sample_ID", "analyte"))

    # this allows for evenly-spaced plots
    bs_df$sample_ID <- as.character(bs_df$sample_ID)

    # put results in report date order
    bs_df <- bs_df[order(bs_df$result_date), ]

    # Check the data
    # correct and correct data structure
    bs_df$result_date <- as.Date(bs_df$result_date)
    bs_df$submission_date <- as.Date(bs_df$submission_date)

    # Spiked Values that are NA are blanks, so no spike value, therefore set to
    # zero
    bs_df$spike_value[is.na(bs_df$spike_value)] <- 0

    # reporting a reading below the detection level is not a false positive
    # false negative = result below detection level when spike is above
    # false positive = result is above detection level when spike is zero

    # identify false negatives and false positives
    bs_df <- bs_df %>%
        dplyr::mutate(err_type =

      # false negative = result below detection level when spike is above
    dplyr::case_when(result < det_lvl & spike_value > det_lvl ~ "false_neg",

      # false positive = result is above detection level when spike is zero
          result > det_lvl & spike_value == 0 ~ "false_pos",
          TRUE ~ "no_err_cat"))

    # check for unit match
    # but only for spiked analytes
    unit_check <- bs_df %>% dplyr::filter(spike_value > 0)
    # make Boolean term for each row
    unit_check$units_same <- unit_check$units == unit_check$spike_unit

     # filter out rows that don't have a mismatch
      unit_check <- unit_check[unit_check$units_same == FALSE, ]
      # remove NA's from unit_check
      unit_check <- unit_check[!is.na(unit_check$units), ]

    if(length(unit_check$units_same) > 0) {
      cat("WARNING: Some units don't match! See table below:")
      cat("\n")
      cat("\n")
      # get a unique set of mismatched units with analytes
      mismatches <- unit_check %>%
        dplyr::select(c(analyte, units, spike_unit))
      mismatch_index <- !duplicated(mismatches)
      mismatches <- data.frame(mismatches[mismatch_index, ])
      names(mismatches) <- c("analyte", "lab unit", "spike unit")
      print.data.frame(mismatches, row.names = F)
    }

    if(length(unit_check$units_same) == 0) {
      cat("Unit check - spike units and lab units all match.")
      cat("\n")
}
          bs_df
}
