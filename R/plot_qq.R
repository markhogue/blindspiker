#' quantile-quantile plot
#'
#' QQ plots by isotope
#'
#' @param my_dir directory where data files are stored in format 'C:/my_data_loc/'.
#' Note the forward slashes.
#'
#' @param compiled_data Data compiled with blindspiker::get_data.R. Defaults to 'df.tsv'
#'
#' @param isotope What is being analyzed
#'
#' @export
plot_qq <- function(my_dir, df = bs_df, isotope) {

    # compiled_data <- paste0(my_dir, '/', compiled_data)

    ISOTOPE <- SPIKED_VALUE <- NULL


    # df <- readr::read_tsv(compiled_data)
    df <- df %>%
        dplyr::filter(ISOTOPE == isotope & SPIKED_VALUE != 0)

    car::qqPlot(df$ACTIVITY, xlab = "Standard Normal Quantiles",
                id = FALSE,
                ylab = paste0(isotope, " in urine (dpm/L)"))

}

