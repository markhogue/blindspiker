#' quantile-quantile plot
#'
#' QQ plots by isotope
#'
#' @param select_analyte the selected analyte for this run chart
#' @param dat data frame with all data needed as described in `get_data`.
#' Default is `bs_df`.
#' @examples
#' example_spike_data <- system.file('extdata', 'spikevals.csv', package = 'blindspiker')
#' example_lab_data <- system.file('extdata', 'labvals.csv', package = 'blindspiker')
#' example_df <- get_data(spike_data = example_spike_data, lab_data = example_lab_data)
#' plot_qq(select_analyte = 'unknownium', dat = example_df)
#'
#' @export
plot_qq <- function(select_analyte,
                    dat = bs_df) {

  # To avoid visible binding note in package check:
  analyte <- result <- spike_unit <- res_to_spike_ratio <- NULL

  df <- dat %>%
    # keep only spiked values
    dplyr::filter(analyte == select_analyte) %>%
    dplyr::filter(spike_value > 0) %>%
    dplyr::filter(result > 0) %>%
    dplyr::mutate(res_to_spike_ratio = result / spike_value)

ggplot2::ggplot(df, ggplot2::aes(sample = res_to_spike_ratio)) +
  ggplot2::geom_qq() +
  ggplot2::geom_qq_line() +
  ggplot2::ggtitle(paste0("quantile-quantile plot of ", select_analyte,
                 " results / spike values"))

}

