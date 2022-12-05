#' Plot turnaround time
#'
#' Plot time for labratory analysis by date (result date from lab - lab received date)
#'
#' @param my_dir directory where data files are stored in format 'C:/my_data_loc/'.
#' Note the forward slashes.
#'
#' @param compiled_data Data compiled with blindspiker::get_data.R.
#'
#' @param isotope What is being analyzed
#'
#' @param target_days The target turnaround time in days. Default = 60.
#'
#' @export
plot_tat <- function(my_dir, df = bs_df, isotope, target_days = 60) {

    # to avoid note error on ggplot args
    ACTIVITY <- `Am-241` <- ISOTOPE <- `Np-237` <- `Pu-238` <- `Pu-239` <- SAMPLE_ID <- SPIKED_VALUE <- Sample <- dtmReceiveDate <- Received <- TAT <- late <- . <- NULL

    # compiled_data <- paste0(my_dir, '/', compiled_data) df <-
    # readr::read_tsv(compiled_data)

    # identify the same date range to be used for all plots in a report
    xlims <- range(c(df$Result, df$dtmReceiveDate))

    df <- df %>%
        dplyr::filter(ISOTOPE == isotope & SPIKED_VALUE != 0)

    df$TAT <- as.numeric(df$Result - df$dtmReceiveDate)

    # df[which(df$TAT<0),] #troubleshooting

    if (any(is.na(df$TAT))) {
        broken <- df %>%
            dplyr::filter(is.na(df$TAT)) %>%
            dplyr::select(SAMPLE_ID) %>%
            as.numeric(.)
        cat(paste0("ID's with date not being read: ", broken))
        df <- df %>%
            dplyr::filter(SAMPLE_ID != broken)
    }

    if (any(df$TAT < 0)) {
        broken <- df %>%
            dplyr::filter(df$TAT < 0) %>%
            dplyr::select(SAMPLE_ID) %>%
            as.numeric(.)
        cat(paste0("ID's with negative TAT's: ", broken))
        df <- df %>%
            dplyr::filter(SAMPLE_ID != broken)
    }


    df$late <- df$TAT > target_days
    lates <- sum(df$late)
    late_pct <- 100 * lates/length(df$TAT)

    # troubleshooting readr::write_tsv(df, 'ts.tsv')

    ggplot2::ggplot(df, ggplot2::aes(dtmReceiveDate, TAT, color = late)) + ggplot2::scale_colour_manual(values = c("black",
        "red")) + ggplot2::xlim(xlims) + ggplot2::geom_point(shape = 1) + ggplot2::geom_segment(ggplot2::aes(xend = dtmReceiveDate,
        yend = 0)) + ggplot2::xlab("receipt date") + ggplot2::ylab("turnaround, days") +
        ggplot2::ggtitle(paste0("Percent ", isotope, " > ", target_days, " days = ",
            signif(late_pct, 3), "%"), subtitle = paste0("Number ", isotope, " > ",
            target_days, " days = ", signif(lates, 3)))
}
