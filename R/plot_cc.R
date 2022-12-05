#' Plot Control Chart
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

plot_cc <- function(my_dir, df = bs_df, isotope) {

    # to avoid note on no visible bindings
    ACTIVITY <- `Am-241` <- ISOTOPE <- `Np-237` <- `Pu-238` <- `Pu-239` <-
      SAMPLE_ID <- SPIKED_VALUE <- Sample <- dtmReceiveDate <- NULL

    df <- df %>%
        dplyr::filter(ISOTOPE == isotope & SPIKED_VALUE != 0)
    df <- df[order(df$SAMPLE_ID), ]

    # f.pos is the fraction of results > spiked
    f.pos <- sum(df$ACTIVITY > df$SPIKED_VALUE)/length(df$SAMPLE_ID)

    CI <- binGroup::binCI(n = length(df$SAMPLE_ID),
                          y = sum(df$ACTIVITY > df$SPIKED_VALUE),
                          conf.level = 0.95,
                          alternative = "two.sided",
                          method = "CP")$conf.int
    CI <- paste0("(", signif(CI[1] * 100, 3), ", ", signif(CI[2] * 100, 3), ")")

    m <- mean(df$ACTIVITY)
    sd2 <- 2 * stats::sd(df$ACTIVITY)

    ggplot2::ggplot(df, ggplot2::aes(dtmReceiveDate, ACTIVITY)) +
      ggplot2::geom_point() +
        ggplot2::geom_line() + ggplot2::xlab("receipt date") +
      ggplot2::scale_x_date(date_labels = "%Y-%m") +
        ggplot2::ylab(paste0(isotope, " in urine (dpm/L)")) +
      ggplot2::geom_hline(yintercept = m,
        color = "black", linetype = 2, size = 0.5) +
      ggplot2::geom_hline(yintercept = 0.05,
        color = "blue", linetype = 3, size = 0.5) +
      ggplot2::ggtitle(paste0(isotope, " blind spike results > 0.05: ",
                  signif(f.pos * 100, 3), "%"),
                  subtitle = paste0("95% CI = ", CI))
}
