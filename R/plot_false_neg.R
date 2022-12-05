#' False Negative Rates
#'
#' Plot False Negative Rates by Isotope
#'
#' @param my_dir directory where data files are stored in format 'C:/my_data_loc/'.
#' Note the forward slashes.
#'
#' @param compiled_data Data compiled with blindspiker::get_data.R. Defaults to 'df.tsv'
#'
#' @param isotope What is being analyzed
#'
#' @export
plot_false_neg <- function(my_dir, df = bs_df, isotope) {

    # to avoid note error on ggplot args
    ACTIVITY <- `Am-241` <- ISOTOPE <- `Np-237` <- `Pu-238` <- `Pu-239` <-
      SAMPLE_ID <- SPIKED_VALUE <- Sample <- value <- measure <- err_type <-
      dtmReceiveDate <- NULL

    # compiled_data <- paste0(my_dir, '/', compiled_data) df <-
    # readr::read_tsv(compiled_data)

    # identify the same date range to be used for all plots in a report
    xlims <- range(c(df$Result, df$dtmReceiveDate))

    df <- df %>%
        dplyr::filter(ISOTOPE == isotope & SPIKED_VALUE != 0)
    df <- df[order(df$SAMPLE_ID), ]
    # df$Sample <- 1:length(df$SAMPLE_ID)

    # compute CI before pivot
    CI <- binGroup::binCI(n = length(df$SAMPLE_ID),
        y = length(df$SAMPLE_ID[which(df$err_type == "false_neg")]),
        conf.level = 0.95, alternative = "two.sided", method = "CP")$conf.int
    CI <- paste0("(", signif(CI[1] * 100, 3), ", ", signif(CI[2] * 100, 3), ")")

    f.neg <- length(df$SAMPLE_ID[which(df$err_type == "false_neg")]) /
      length(df$SAMPLE_ID)



    df <- df %>%
        tidyr::pivot_longer(cols = c("ACTIVITY", "DET_LEVEL"), names_to = "measure",
            values_to = "value")

    ggplot2::ggplot(df, ggplot2::aes(dtmReceiveDate, value, color = err_type)) +
      ggplot2::xlab("receipt date") +
      ggplot2::scale_x_date(date_labels = "%Y-%m") +
      ggplot2::scale_colour_manual(values = c("red", "black")) +
      ggplot2::scale_shape_manual(values = c(1, 2)) +
      ggplot2::geom_point(size = 1) +
      ggplot2::geom_point(ggplot2::aes(shape = err_type),
        size = 1.5, alpha = 0.7) +
      ggplot2::geom_hline(yintercept = 0.05, color = "black",
        linetype = 2, size = 0.5) +
      ggplot2::ylab(paste0(isotope, " in urine (dpm/L)")) +
      ggplot2::ggtitle(paste0(isotope, " false negative rate: ", " = ",
              signif(f.neg * 100, 3), "%"),
              subtitle = paste0("95% CI = ", CI)) +
      ggplot2::facet_grid(rows = ggplot2::vars(measure),
        scales = "free_y")
}
