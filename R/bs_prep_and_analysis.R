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
#' \itemize{
#' \item `sample_ID` unique identifier, character or numeric
#' \item `analyte` character data
#' \item `spike_value` numeric value
#' \item `spike_units` character data
#' \item `submission_date` character data that will be converted to date in format
#'  YYYY-MM-DD (for example 1999-12-31)
#'  }
#'
#' Optional for spike data:
#' \itemize{
#' \item `sv_unc` numeric, the uncertainty of the spike value. Default = 0
#' \item `sv_k` the coverage factor for the spike value uncertainty. Default = 2
#' \item `provider lab` character name of laboratory providing spiked samples
#'}
#'
#' Required for laboratory results:
#' \itemize{
#' \item `sample_ID` must match spike `sample_ID`
#' \item `analyte` must match spike `analyte`
#' \item `result` numeric value
#' \item `units` must match `spike_units`
#' \item `result_date`
#' \item `det_lvl` numeric detection level
#' \item `unc` numeric uncertainty of the laboratory result
#'}
#'
#'#' Optional for lab data:
#' \itemize{
#' \item `k` the coverage factor for the result uncertainty. Default = 2
#'}
#' Note that the two data sets (spike values and laboratory results) will
#' be combined by `sample_ID`, and also by `analyte` if present in both sets.
#' (If the laboratory data includes a non-zero result for an analyte not
#' present in the spike data, that would indicate a false positive.)
#'
#' Use this function to load the spike data and establish a data frame named
#' `bs_df`. All plotting and data analysis functions will default to look for
#' this data set.
#'
#' @param spike_data name of the loaded dataset (no quotes) or the name
#' of the file, with path, containing the spike values, in quotes.
#' Example forms, my_spike_data, or "C:/my_directory/my_spike_data.csv".
#' This file column headings must be as identified in the Details section.
#'
#' @param lab_data name of the loaded dataset (no quotes) or the name
#' of the file, with path, containing the laboratory results.
#' Example forms, my_lab_data, or "C:/my_directory/my_lab_data.csv".
#' This file column headings must be as identified in the Details section.
#'
#' @return data frame containing all needed data to be used in subsequent
#' functions.
#'
#' @examples
#' example_spike_data <- system.file("extdata", "spikevals.csv", package = "blindspiker")
#' example_lab_data <- system.file("extdata", "labvals.csv", package = "blindspiker")
#' example_df <- bs_prep_and_analysis(spike_data = example_spike_data, lab_data = example_lab_data)
#'
#' @export

bs_prep_and_analysis <- function(spike_data,
                           lab_data) {

  analyte <- units <- spike_unit <- spike_value <- sv_unc <- sv_k <- spike_value <- NULL

  #load spike data
  # Is the data already loaded or do we go get it?
  # If spike_data is entered as character name of a data file:
  if(is.character(spike_data))  spike_df <- utils::read.csv(file = spike_data)

  # Otherwise, it's already loaded, let's use it!
  if(!is.character(spike_data)) spike_df <- spike_data

  # check spike names
  spike_names_check <- names(spike_df) %in%
    c("sample_ID", "analyte", "spike_value",
      "sv_unc", "sv_k", "spike_unit",
      "provider_lab", "submission_date")

  if (!all(spike_names_check) == TRUE) {
    message("Column names in spike data do not all match, \n")
    message("Spike data names are: \n")
    message(names(spike_df))
    message("\n")
    message("Allowed names are: \n")
    message(c("sample_ID", "analyte", "spike_value", "sv_unc",
            "sv_k", "spike_unit", "provider_lab", "submission_date"))
    message("\n")
    message("This name was unrecognized: \n")
    message(names(spike_df)[which(spike_names_check == F)])
    stopifnot(all(spike_names_check) == TRUE)
  }

  # set defaults where arguments not provided
  #if (is.null(spike_df$sv_unc))
  if(!"sv_unc" %in% names(spike_df))
    spike_df$sv_unc <- 0
  if(!"sv_k" %in% names(spike_df))
    spike_df$sv_k <- 2

  # load lab data
  if(is.character(lab_data))  lab_df <- utils::read.csv(file = lab_data)
  if(!is.character(lab_data)) lab_df <- lab_data

  lab_names_check <- names(lab_df) %in%
    c("sample_ID", "analyte", "result_date",
      "k", "result", "unc", "units", "det_lvl")

  # check lab names
  if (!all(lab_names_check) == TRUE) {
    message("Column names in laboratory data do not all match, \n")
    message("Laboratory data names are: \n")
    message(names(lab_df))
    message("\n")
    message("Allowed names are: \n")
    message(c("sample_ID", "analyte", "result_date", "k", "result",
            "unc", "units", "det_lvl"))
    message("\n")
    message("This name was unrecognized: \n")
    message(names(lab_df)[which(lab_names_check == F)])
    stopifnot(all(lab_names_check) == TRUE)
  }

  # set lab defaults if arguments not passed
  #if (is.null(lab_df$k))
  if(!"k" %in% names(lab_df))
    lab_df$k <- 2

  # check analyte match between lab and spike
  spike_analytes <- unique(spike_df$analyte)
  lab_analytes <- unique(lab_df$analyte)
  analyte_mismatch_ind <- !lab_analytes %in% spike_analytes
  analyte_mismatch <- lab_analytes[analyte_mismatch_ind]
  if (any(analyte_mismatch_ind == TRUE)) {
    message(paste0("WARNING: ", analyte_mismatch, " is reported by the lab, but is not in spike data. \n"))
  }
  else {
    message("Analytes check - lab data and spike analytes all match.")
  }
  message("\n")
  message("\n")

  # combine lab and spike data
  bs_df <- dplyr::full_join(spike_df, lab_df, by = c("sample_ID",
                                                     "analyte"))

  # Check for duplicated data
  dup_check <- duplicated(data.frame(bs_df$sample_ID, bs_df$analyte))
  if (any(dup_check == TRUE)) {
    message(paste0("sample_ID ",
               bs_df$sample_ID[which(dup_check == TRUE)], " is a duplicate"))
    stopifnot(`Duplicated sample_ID for same analyte` =
                all(dup_check == FALSE))
  }

  # fix data types
  bs_df$sample_ID <- as.character(bs_df$sample_ID)
  bs_df <- bs_df[order(bs_df$result_date), ]
  bs_df$result_date <- as.Date(bs_df$result_date)
  bs_df$submission_date <- as.Date(bs_df$submission_date)

  # if no spike value, make it 0
  bs_df$spike_value[is.na(bs_df$spike_value)] <- 0

  # err_type generation
  bs_df <- bs_df %>% dplyr::mutate(
    err_type = dplyr::case_when(
      result < det_lvl & spike_value > det_lvl ~ "false_neg",
      result > det_lvl & spike_value == 0 ~ "false_pos",
      TRUE ~ "no_err_cat")
  )

  # add overlap details, then we'll have one more error cat
  bs_df <- bs_df %>%
    dplyr::mutate(low_res = dplyr::case_when(
      result > det_lvl ~ result - (unc * 2/k),
      TRUE ~ result)) %>%
    dplyr::mutate(high_res = dplyr::case_when(result >
                                                det_lvl ~ result + (unc * 2/k), TRUE ~ result)) %>%
    dplyr::mutate(low_spike = spike_value - (sv_unc *
                                               2/sv_k)) %>% dplyr::mutate(high_spike = spike_value +
                                                                            (sv_unc * 2/sv_k)) %>%

    # spike overlap codes: 1 = no overlap
    #                      0 = overlap

    dplyr::mutate(spike_overlap = as.factor(
      dplyr::case_when(low_res > high_spike ~ 1,
                       high_res < low_spike ~ 1,
                       TRUE ~ 0)))

  # add error category
  bs_df$err_type[which(bs_df$err_type == "no_err_cat" &
                         bs_df$spike_overlap == 1 &
                         bs_df$spike_value > 0)] <- "range"

  # check that units match

  unit_check <- bs_df %>% dplyr::filter(spike_value > 0)
  unit_check$units_same <- unit_check$units == unit_check$spike_unit
  unit_check <- unit_check[unit_check$units_same == FALSE, ]
  unit_check <- unit_check[!is.na(unit_check$units), ]
  if (length(unit_check$units_same) > 0) {
    message("WARNING: Some units don't match! See table below:")
    message("\n")
    message("\n")
    mismatches <- unit_check %>% dplyr::select(c(analyte,
                                                 units, spike_unit))
    mismatch_index <- !duplicated(mismatches)
    mismatches <- data.frame(mismatches[mismatch_index, ])
    names(mismatches) <- c("analyte", "lab unit", "spike unit")
    message(mismatches)
  }
  if (length(unit_check$units_same) == 0) {
    message("Unit check - spike units and lab units all match.")
    message("\n")
  }
  bs_df
}
