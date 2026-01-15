#' Plot Run Chart
#'
#' @description
#' `plot_run()` produces a Run Chart of the selected analyte. The analyte
#' is selected from the analyte set provided. Results are plotted with error
#' bars (uncertainty with coverage factor of 2) when the result is greater than
#' the detection level. On the ratio version, the uncertainties for the results
#' and spike values are combined as the square root of the sums of the relative
#' uncertainties squared. When original results are plotted, the spike values
#' are shown with a small salmon-colored "+".
#'
#' @param select_analyte the selected analyte for this run chart
#' @param dat data frame with all data needed as described in `bs_prep_and_analysis`.
#' Default is `bs_df`.
#' @param version The run chart is either shown with `original` units, default,
#' or with the result shown as a `ratio` to the spike value.
#' @param log Set log = "y" to make the y-axis a log scale - original version
#' only. Default is "n".
#' @param removal_notification provide a list of results, by
#'  sample_ID that were removed from the current run plot because
#'  results were less than or equal to zero.
#'
#' @examples
#' example_spike_data <- system.file('extdata', 'spikevals.csv', package = 'blindspiker')
#' example_lab_data <- system.file('extdata', 'labvals.csv', package = 'blindspiker')
#' example_df <- bs_prep_and_analysis(spike_data = example_spike_data, lab_data = example_lab_data)
#' plot_run(select_analyte = 'unknownium', dat = example_df)
#'
#' @export

plot_run <- function(select_analyte,
                     dat = bs_df,
                     version = "original",
                     log = "n",
                     removal_notification = "n") {
  analyte <- result <- unc <- sample_ID <- res_to_spike_ratio <- low_res <- high_res <- low_rat <- high_rat <- bs_df <- spike_overlap <- spike_value <- sv_unc <- k <- sv_k <- low_spike <- high_spike <- NULL

  if (!version %in% c("ratio", "original"))
    version <- "original"

  if (version == "original") {
    df2 <- dat %>%
      dplyr::filter(analyte == select_analyte)
    unit_txt <- df2$units[1]

    # If a result is negative, we'll remove it from this function and tell
    # user it was removed if desired.
    if (length(df2$sample_ID[df2$result <= 0]) > 0) {
      if(removal_notification == "y") {
        cat("Removing results <= 0 for the following samples, \n")
        print.data.frame(data.frame(sample_ID = df2$sample_ID[df2$result <= 0]))}
    }

    df2 <- df2 %>%
      dplyr::filter(result > 0) %>%
      dplyr::mutate(low_res = dplyr::case_when(result > det_lvl ~ result -
                                                 (unc * 2/k),
                                               TRUE ~ result)) %>%
      dplyr::mutate(high_res = dplyr::case_when(result > det_lvl ~ result +
                                                  (unc * 2/k),
                                                TRUE ~ result)) %>%
      dplyr::mutate(low_spike = spike_value - (sv_unc * 2/sv_k)) %>%
      dplyr::mutate(high_spike = spike_value + (sv_unc * 2/sv_k)) %>%
      dplyr::mutate(spike_overlap =
                      as.factor(dplyr::case_when(low_res > high_spike ~ 1,
                                                 high_res < low_spike ~ 1,
                                                 TRUE ~ 0)))

    df2 <- df2[order(df2$submission_date), ]
    date_range <- (range(df2$submission_date))
    p2 <- ggplot2::ggplot(data = df2,
                          ggplot2::aes(sample_ID, result, color = spike_overlap)) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::geom_linerange(data = df2,
                              ggplot2::aes(x = sample_ID,
                                           ymin = low_res,
                                           ymax = high_res,
                                           color = spike_overlap),
                              linewidth = 0.5, show.legend = FALSE) +
      ggplot2::geom_point(ggplot2::aes(sample_ID,
                                       spike_value),
                          shape = 3,
                          size = 0.8,
                          color = "darksalmon") +
      ggplot2::scale_color_manual(values = c("black", "red")) +
      ggplot2::xlab("results in submission date order") +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::ylab(paste0("result, ",
                           unit_txt)) +
      ggplot2::ggtitle(paste0("run plot for ", select_analyte)) +
      ggplot2::labs(caption = paste0("submission date range ",
                                     date_range[1], " to ",
                                     date_range[2]))

    if (log == "y")
      p2 <- p2 + ggplot2::scale_y_log10()
    p <- p2
    p
  }
  if (version == "ratio") {
    df <- dat %>%
      dplyr::filter(analyte == select_analyte) %>%
      dplyr::filter(spike_value > 0)
    unit_txt <- df$units[1]
    if (length(df$sample_ID[df$result <= 0]) > 0) {
      if(removal_notification == "y") {
        cat("Removing results <= 0 for the following samples, \n")
        print.data.frame(data.frame(sample_ID = df$sample_ID[df$result <= 0]))
      }
    }
    df <- df %>%
      dplyr::filter(result > 0) %>%
      dplyr::mutate(res_to_spike_ratio = result/spike_value) %>%
      dplyr::mutate(rat_unc = sqrt(((unc * 2/k)/result)^2) +
                      ((sv_unc * 2/sv_k)/spike_value)^2) %>%
      dplyr::mutate(low_rat =
                      dplyr::case_when(result > det_lvl ~
                                         (res_to_spike_ratio - (rat_unc * 2/k)),
                                       TRUE ~ res_to_spike_ratio)) %>%
      dplyr::mutate(high_rat =
                      dplyr::case_when(result > det_lvl ~
                                         (res_to_spike_ratio + (rat_unc * 2/k)),
                                       TRUE ~ res_to_spike_ratio)) %>%
      dplyr::mutate(spike_overlap = as.factor(
        dplyr::case_when(high_rat < 1 ~ 1,
                         low_rat > 1 ~ 1,
                         TRUE ~ 0)))
    df$res_to_spike_ratio[is.infinite(df$res_to_spike_ratio)] <-
      df$low_rat[is.nan(df$low_rat)] <- 0
    df$high_rat[is.nan(df$high_rat)] <- 0
    df$high_rat[is.infinite(df$high_rat)] <- NA
    df <- df[order(df$submission_date), ]
    date_range <- (range(df$submission_date))
    p0 <- ggplot2::ggplot(data = df,
                          ggplot2::aes(sample_ID, res_to_spike_ratio,
                                       ymin = low_rat,
                                       ymax = high_rat,
                                       color = spike_overlap)) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::geom_pointrange() +
      ggplot2::scale_color_manual(values = c("black", "red")) +
      ggplot2::geom_hline(yintercept = 1,
                          color = "darksalmon", linetype = 2) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::xlab("results in submission date order") +
      ggplot2::ylab(paste0(select_analyte, " result / spike value")) +
      ggplot2::ggtitle(paste0(select_analyte,
                              " result / spike")) +
      ggplot2::labs(caption = paste0("submission date range ",
                                     date_range[1], " to ", date_range[2]))
    if (log == "y")
      cat("log argument ignored for ratio version, \n")
    p <- p0
  }
  p
}
