#' Plot turnaround time
#'
#' Plot time for laboratory analysis by date (result date from lab - spiked sample submitted date)
#'
#' @param select_analyte the selected analyte for this run chart
#' @param dat data frame with all data needed as described in `bs_prep_and_analysis`.
#' Default is `bs_df`.
#' @param target_days The target turnaround time in days. Default = 60.
#'
#' @return turn-around-time plot
#'
#' @examples
#' example_spike_data <- system.file('extdata', 'spikevals.csv', package = 'blindspiker')
#' example_lab_data <- system.file('extdata', 'labvals.csv', package = 'blindspiker')
#' example_df <- bs_prep_and_analysis(spike_data = example_spike_data, lab_data = example_lab_data)
#' plot_tat(select_analyte = 'unknownium', dat = example_df, target_days = 60)
#'
#' @export
plot_tat <- function(select_analyte, dat = bs_df, target_days = 60) {

  analyte <- spike_value <- sample_ID <- tat <- submission_date <- late <- bs_df <- k <- NULL

  df <- dat %>% dplyr::filter(analyte == select_analyte &
                                spike_value != 0)
  df$tat <- as.numeric(df$result_date - df$submission_date)

  if (any(is.na(df$tat))) {
    broken <- df %>% dplyr::filter(is.na(df$tat)) %>%
      dplyr::select(sample_ID)
    cat(paste0("ID's with date not being read: ", broken))
    df <- df %>% dplyr::filter(!is.na(tat))
  }

  if (any(df$tat < 0)) {
    broken <- df %>% dplyr::filter(df$tat < 0)
    cat(paste0("ID's with negative tat's: ", broken$sample_ID, "\n"))
  }


  df$late <- df$tat > target_days
  lates <- sum(df$late)
  late_pct <- 100 * lates/length(df$tat)
  ggplot2::ggplot(df, ggplot2::aes(submission_date, tat, color = late)) +
    ggplot2::scale_colour_manual(values = c("black", "red")) +
    ggplot2::geom_point(shape = 1) + ggplot2::geom_segment(ggplot2::aes(xend = submission_date,
                                                                        yend = 0)) + ggplot2::xlab("receipt date") + ggplot2::ylab("turnaround, days") +
    ggplot2::ggtitle(paste0("Percent ", select_analyte, " > ",
                            target_days, " days = ", signif(late_pct, 3), "%"),
                     subtitle = paste0("Number ", select_analyte, " > ",
                                       target_days, " days = ", signif(lates, 3)))
}
