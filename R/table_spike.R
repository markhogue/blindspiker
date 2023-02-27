#' Make table summarizing the blind spikes.
#'
#' Make a table showing how many blind spike samples contained one or more analytes.
#'
#' @param df data frame with all data needed as described in `get_data`.
#'
#' @seealso For combinations of analytes spiked in a single sample,
#' see `spike_combos`.
#'
#' @examples
#' example_spike_data <- system.file('extdata', 'spikevals.csv', package = 'blindspiker')
#' example_lab_data <- system.file('extdata', 'labvals.csv', package = 'blindspiker')
#' example_df <- get_data(spike_data = example_spike_data, lab_data = example_lab_data)
#' table_spike(df = example_df)
#'
#' @export
#'
table_spike <- function(df = bs_df) {

  ind_anal_tbl <- example_df %>% dplyr::filter(spike_value == 0) %>%
    dplyr::group_by(analyte) %>%
    dplyr::summarise(count = dplyr::n())

  gt::gt(ind_anal_tbl) %>% gt::tab_header("analytes in spiked samples, this data set")
}