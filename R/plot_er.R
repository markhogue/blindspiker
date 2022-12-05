#' Plot error rates
#'
#' Plot Control Charts by Isotope
#'
#' @param my_dir directory where data files are stored in format 'C:/my_data_loc/'.
#' Note the forward slashes.
#'
#' @param compiled_data Data compiled with blindspiker::get_data.R. Defaults to 'df.tsv'
#'
#' @param isotope What is being analyzed
#'
#' @export
plot_er <- function(my_dir, df = bs_df, isotope) {

    # to avoid note error on ggplot args
    ACTIVITY <- `Am-241` <- ISOTOPE <- `Np-237` <- `Pu-238` <- `Pu-239` <- SAMPLE_ID <- SPIKED_VALUE <- Sample <- err_coverage <- lower <- upper <- dtmReceiveDate <- NULL

    # compiled_data <- paste0(my_dir, '/', compiled_data) df <-
    # readr::read_tsv(compiled_data)

    # identify the same date range to be used for all plots in a report
    xlims <- range(c(df$Result, df$dtmReceiveDate))

    df <- df %>%
        dplyr::filter(ISOTOPE == isotope & SPIKED_VALUE != 0)
    df <- df[order(df$SAMPLE_ID), ]
    # df$Sample <- 1:length(df$SAMPLE_ID)

    df$upper <- df$ACTIVITY + 2 * df$ERROR
    df$lower <- df$ACTIVITY - 2 * df$ERROR

    df$err_coverage <- dplyr::case_when(df$upper < df$SPIKED_VALUE ~ 1, df$lower >
        df$SPIKED_VALUE ~ 1, TRUE ~ 0)

    # length(df$SAMPLE_ID[which(df$upper < df$SPIKED_VALUE | df$lower >
    # df$SPIKED_VALUE)])
    err_rate <- sum(df$err_coverage)/length(df$SAMPLE_ID)

    # compute CI before pivot
    CI <- binGroup::binCI(n = length(df$SAMPLE_ID), y = sum(df$err_coverage), conf.level = 0.95,
        alternative = "two.sided", method = "CP")$conf.int

    CI <- paste0("(", signif(CI[1] * 100, 3), ", ", signif(CI[2] * 100, 3), ")")

    df$err_coverage <- as.factor(df$err_coverage)

    ggplot2::ggplot(df, ggplot2::aes(dtmReceiveDate, ACTIVITY, color = err_coverage)) +
        ggplot2::xlab("receipt date") + ggplot2::scale_x_date(date_labels = "%Y-%m") +
        ggplot2::scale_colour_manual(values = c("black", "red")) + ggplot2::geom_point() +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0) +
        ggplot2::geom_hline(yintercept = 0.05, color = "black", linetype = 2, size = 0.5) +
        ggplot2::ylab(paste0(isotope, " in urine (dpm/L)")) + ggplot2::theme(legend.position = "none") +
        ggplot2::ggtitle(paste0(isotope, " error Rate: ", signif(err_rate * 100,
            3), "%"), subtitle = paste0("95% CI = ", CI))
}
