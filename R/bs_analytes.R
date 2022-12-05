#' Tabulate analytes submitted
#'
#' Provide table of analyte mixtures
#'
#' @param my_dir directory where data files are stored in format 'C:/my_data_loc/'.
#' Note the forward slashes.
#'
#' @param compiled_data Data compiled with blindspiker::get_data.R. Defaults to 'df.tsv'
#'
#' @export
bs_analytes <- function(my_dir, df = bs_df) {
    # to avoid note on no visible bindings
    ACTIVITY <- `Am-241` <- ISOTOPE <- `Np-237` <- `Pu-238` <- `Pu-239` <- SAMPLE_ID <- SPIKED_VALUE <- Sample <- NULL


    # compiled_data <- paste0(my_dir, '/', compiled_data) df <-
    # readr::read_tsv(compiled_data)

    # remove the blanks
    df <- df %>%
        dplyr::filter(SPIKED_VALUE != 0) %>%
        dplyr::select(c(SAMPLE_ID, ISOTOPE, SPIKED_VALUE)) %>%
        tidyr::pivot_wider(names_from = ISOTOPE, values_from = SPIKED_VALUE)
    Pu_Am <- df %>%
        dplyr::filter(!is.na(`Am-241`) & (!is.na(`Pu-238`) | !is.na(`Pu-239`)) &
            is.na(`Np-237`))
    Pu_Am_Np <- df %>%
        dplyr::filter(!is.na(`Am-241`) & (!is.na(`Pu-238`) | !is.na(`Pu-239`)) &
            !is.na(`Np-237`))
    Pu_Np <- df %>%
        dplyr::filter(is.na(`Am-241`) & (!is.na(`Pu-238`) | !is.na(`Pu-239`)) & !is.na(`Np-237`))
    Pu_only <- df %>%
        dplyr::filter(is.na(`Am-241`) & (!is.na(`Pu-238`) | !is.na(`Pu-239`)) & is.na(`Np-237`))
    data.frame(Combination = c("Pu-Am", "Pu-Am-Np", "Pu-Np", "Pu-only"), Count = c(length(Pu_Am$SAMPLE_ID),
        length(Pu_Am_Np$SAMPLE_ID), length(Pu_Np$SAMPLE_ID), length(Pu_only$SAMPLE_ID)))
}
