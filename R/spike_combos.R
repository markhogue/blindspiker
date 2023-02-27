#' Count combinations of spiked samples
#'
#' @description
#' A summary of spiked samples is provided based on combinations of interest.
#'
#' @param dat data frame with all data needed as described in `get_data`.
#' Default is `bs_df`.
#'
#' @param analytes a vector of analytes of interest
#'
#' @examples
#' example_spike_data <- system.file('extdata', 'spikevals.csv', package = 'blindspiker')
#' example_lab_data <- system.file('extdata', 'labvals.csv', package = 'blindspiker')
#' example_df <- get_data(spike_data = example_spike_data, lab_data = example_lab_data)
#' spike_combos(analytes = c('unknownium', 'Sr-90'), df = example_df)
#'
#' @export
spike_combos <- function(analytes, dat = bs_df){

  # To avoid visible binding note in package check:

  analyte <- all_of <- bs_df <- spike_value <- NULL

      # get all spike data from dat
  # selected columns are sample_ID, analyte, and spike_value
  spikes <- dat[dat$spike_value > 0, 1:3]

  # make a data frame with analytes as column names
  spikes_wide <- spikes %>%
    tidyr::pivot_wider(
      names_from = analyte,
      values_from = spike_value)

  cols_to_count <- which(names(spikes_wide) %in% analytes)

    just_analyte_cols <- spikes_wide %>%
    dplyr::select(all_of(cols_to_count))

    cat("This is a list of all spiked samples meeting the selection criteria,
        \n")

  print.data.frame(spikes_wide[(stats::complete.cases(just_analyte_cols)),
                               c(1, cols_to_count)])

  cat(paste0("The total number of spiked samples meeting the selction criteria is: ",
  length(spikes_wide$sample_ID[(stats::complete.cases(just_analyte_cols))])))

}
