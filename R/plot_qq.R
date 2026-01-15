#' quantile-quantile plot
#'
#' QQ plots by isotope
#'
#' @param select_analyte the selected analyte for this run chart
#' @param dat data frame with all data needed as described in `bs_prep_and_analysis`.
#' Default is `bs_df`.
#' @examples
#' example_spike_data <- system.file('extdata', 'spikevals.csv', package = 'blindspiker')
#' example_lab_data <- system.file('extdata', 'labvals.csv', package = 'blindspiker')
#' example_df <- bs_prep_and_analysis(spike_data = example_spike_data, lab_data = example_lab_data)
#' plot_qq(select_analyte = 'unknownium', dat = example_df)
#'
#' @export
plot_qq <- function(select_analyte,
                    dat = bs_df) {
  analyte <- result <- spike_unit <- res_to_spike_ratio <- spike_value <- bs_df <- NULL

  df <- dat %>% dplyr::filter(analyte == select_analyte) %>%
    dplyr::filter(spike_value > 0) %>%
    dplyr::filter(result > 0) %>%
    dplyr::mutate(res_to_spike_ratio = result/spike_value)

  stats::qqnorm(df$result[df$analyte == select_analyte & df$spike_value > 0],
         main = paste0(select_analyte, " results"))
  stats::qqline(df$result[df$analyte == select_analyte & df$spike_value > 0])
}
