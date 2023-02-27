#' Make table summarizing the blind spikes.
#'
#' Make a table showing how many blind spike samples contained one or more analytes.
#'
#' @param dat data frame with all data needed as described in `get_data`.
#'
#' @seealso For combinations of analytes spiked in a single sample,
#' see `spike_combos`.
#'
#' @examples
#' example_spike_data <- system.file('extdata', 'spikevals.csv', package = 'blindspiker')
#' example_lab_data <- system.file('extdata', 'labvals.csv', package = 'blindspiker')
#' example_df <- get_data(spike_data = example_spike_data, lab_data = example_lab_data)
#' table_spike(dat = example_df)
#'
#' @export
#'
table_spike <- function(dat = bs_df) {

  # To avoid visible binding note in package check:
  bs_df <- result <- err_type <- det_lvl <- analyte <- spike_value <- NULL

  ind_anal_tbl <- df %>% dplyr::filter(spike_value == 0) %>%

    dplyr::group_by(analyte) %>%
    dplyr::summarise(count = dplyr::n())

  gt::gt(ind_anal_tbl) %>% gt::tab_header("analytes in spiked samples, this data set")
}
