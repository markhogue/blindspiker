#' Plot Run Chart
#'
#' @description
#' `plot_run()` produces a Run Chart of the selected analyte. The analyte
#' is selected from the analyte set provided. Results are plotted with error
#' bars (uncertainty with coverage factor of 2) when the result is greater than
#' the detection level. When original results are plotted, the spike values are
#' shown with a small salmon-colored "+".
#'
#' @param select_analyte the selected analyte for this run chart
#' @param dat data frame with all data needed as described in `get_data`.
#' Default is `bs_df`.
#' @param matrix what the analyte is suspended in. For example, 'water'. Default
#' is 'sample'.
#' @param version The run chart is either shown with `original` units, default, or
#' with the result shown as a `ratio` to the spike value.
#' @param log Set log = "y" to make the y-axis a log scale. Default is "n".
#'
#' @examples
#' example_spike_data <- system.file('extdata', 'spikevals.csv', package = 'blindspiker')
#' example_lab_data <- system.file('extdata', 'labvals.csv', package = 'blindspiker')
#' example_df <- get_data(spike_data = example_spike_data, lab_data = example_lab_data)
#' plot_run(select_analyte = 'unknownium', dat = example_df)
#'
#' @export

plot_run <- function(select_analyte,
                     dat = bs_df,
                     version = "ratio",
                     matrix = "sample",
                     log = "n")
  {

  analyte <- result <- unc <- sample_ID <- res_to_spike_ratio <- low_res <-
  high_res <- low_rat <- up_rat <- bs_df <- spike_overlap <- spike_value <-
  k <-  NULL

  # If matrix option is changed, but 'original' is misspelled or missing,
  # change to 'original'
    if(!version %in% c("ratio", "original"))
      version <- "original"

    if(version == "original"){

    df2 <- dat %>%
      # keep only spiked values
      dplyr::filter(analyte == select_analyte)
    unit_txt <- df2$units[1]
    # remove results = 0
    if(length(df2$sample_ID[df2$result <= 0]) > 0) {
      cat("Removing results <= 0 for the following samples, \n")
      print.data.frame(data.frame("sample_ID" = df2$sample_ID[df2$result <= 0]))
    }

    df2 <- df2 %>%
      dplyr::filter(result > 0) %>%

      dplyr::mutate(
        low_res = dplyr::case_when(
          result > det_lvl ~
            result - (unc * 2 / k),
          TRUE ~ result)) %>%

      dplyr::mutate(
        up_res = dplyr::case_when(
          result > det_lvl ~
            result + (unc * 2 / k),
          TRUE ~ result)) %>%
       dplyr::mutate(spike_overlap =
        as.factor(
        dplyr::case_when(low_res > spike_value ~ 1,
                                         high_res < spike_value ~1,
                                         TRUE ~ 0)))

    df2 <- df2[order(df2$result_date), ]
    date_range <- (range(df2$result_date))

    p <- ggplot2::ggplot(data = df2,
            ggplot2::aes(sample_ID,
                        result,
                        color = spike_overlap)) +
     ggplot2::geom_point(shape = 1) +
     ggplot2::geom_linerange(data = df2,
     ggplot2::aes(x= sample_ID,
                    ymin = low_res,
                    ymax = high_res,
                    color = spike_overlap),
                    linewidth = 0.5,
                     show.legend = FALSE) +
     ggplot2::geom_point(ggplot2::aes(sample_ID, spike_value),
                         shape = 3,
                         size = 0.8,
                         color = "darksalmon") +
     ggplot2::scale_color_manual(values =  c("black", "red")) +
        ggplot2::xlab("results in report date order") +
     ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                    axis.ticks = ggplot2::element_blank(),
                    legend.position = "none") +
     ggplot2::ylab(paste0("result, ", unit_txt)) +
        ggplot2::ggtitle(paste0("run plot for ", select_analyte)) +
        ggplot2::labs(caption = paste0("result date range ", date_range[1],
                                    " to ", date_range[2]))

   if(log == "y") p <- p + ggplot2::scale_y_log10()
    p
    }


# ratio version -----------------------------------------------------------


if(version == "ratio"){
  df <- dat %>%
    # keep only spiked values
    dplyr::filter(analyte == select_analyte) %>%
    dplyr::filter(spike_value > 0)

  unit_txt <- df$units[1]
  # remove results = 0
  if(length(df$sample_ID[df$result <= 0]) > 0) {
    cat("Removing results <= 0 for the following samples, \n")
    print.data.frame(data.frame("sample_ID" = df$sample_ID[df$result <= 0]))
  }

  df <- df %>%
    dplyr::filter(result > 0) %>%
    dplyr::mutate(res_to_spike_ratio = result/spike_value) %>%

      dplyr::mutate(
      low_rat = dplyr::case_when(
        result > det_lvl ~
        result - (unc * 2 / k)  / spike_value,
        TRUE ~ result)) %>%

    dplyr::mutate(
      up_rat = dplyr::case_when(
        result > det_lvl ~
        result + (unc * 2 / k)  / spike_value,
        TRUE ~ result)) %>%

    dplyr::mutate(spike_overlap =
                    as.factor(
                      dplyr::case_when(up_rat < 1 ~ 1,
                                       low_rat > 1 ~1,
                                       TRUE ~ 0)
                    )
    )

  df$low_rat[is.nan(df$lower)] <- 0
  df$up_rat[is.nan(df$upper)] <- 0

  df <- df[order(df$result_date), ]
  date_range <- (range(df$result_date))

p <- ggplot2::ggplot(data = df,
                     ggplot2::aes(sample_ID,
                                  res_to_spike_ratio,
                                  color = spike_overlap)) +
    ggplot2::geom_point(shape = 1) +
    ggplot2::geom_linerange(ggplot2::aes(x = sample_ID,
                                       ymin = low_rat,
                                       ymax = up_rat,
                                       color = spike_overlap),
                          linewidth = 0.5,
                          show.legend = FALSE) +
  ggplot2::scale_color_manual(values =  c("black", "red")) +
  ggplot2::geom_hline(yintercept = 1,
                      color = "darksalmon",
                      linetype = 2) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   legend.position = "none")  +
    ggplot2::xlab("results in report date order") +
    ggplot2::ylab(paste0(select_analyte, " result / spike value")) +
    ggplot2::ggtitle(paste0(select_analyte, " result / spike")) +
      ggplot2::labs(caption = paste0("result date range ", date_range[1],
                                     " to ", date_range[2]))

     if(log == "y") p <- p + ggplot2::scale_y_log10()
p
}
p
}
