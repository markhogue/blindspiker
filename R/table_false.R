#' Make table of false positives and false negatives in laboratory results
#'
#' Make a table with estimated confidence intervals for false negatives and
#' false positives for an analyte in the data set.
#'
#' False negatives are the number of laboratory results that missed a spiked
#' value. For the false negative rate, the numerator is the number of laboratory
#' results less than detection level for spiked samples. The denominator is the
#' number or spiked samples.
#'
#' False positives are the number of laboratory results above detection level
#' when the analyte identified by the laboratory was not in the spiked sample.
#' For the false positive rate, the numerator is the number of false positives.
#' The denominator is the number of false positives plus number of true
#' negatives.
#'
#' The total error rate is the total number of laboratory results with either
#' false negative or false positive results divided by the total number of
#' laboratory results.
#'
#' @param select_analyte the selected analyte for this table
#' @param dat data frame with all data needed as described in `bs_prep_and_analysis`.
#'
#' @examples
#' example_spike_data <- system.file('extdata', 'spikevals.csv', package = 'blindspiker')
#' example_lab_data <- system.file('extdata', 'labvals.csv', package = 'blindspiker')
#' example_df <- bs_prep_and_analysis(spike_data = example_spike_data, lab_data = example_lab_data)
#' table_false(select_analyte = "Sr-90", dat = example_df)
#'
#' @export
#'
table_false <- function (select_analyte, dat = bs_df) {
  result <- err_type <- det_lvl <- analyte <- bs_df <- . <- spike_value <- NULL

  my_analytes <- unique(dat$analyte)

  stopifnot(`Argument, select_analyte, is not in the data set.` = select_analyte %in% my_analytes)

  df <- dat %>% dplyr::filter(analyte == select_analyte)

  n_true_neg <- df %>% dplyr::filter(analyte == select_analyte) %>%
    dplyr::filter(err_type == "no_err_cat") %>%
    dplyr::filter(result < det_lvl) %>%
    dplyr::count(.) %>% as.numeric()

  n_false_neg <- df %>%
    dplyr::filter(analyte == select_analyte) %>%
    dplyr::filter(err_type == "false_neg") %>%
    dplyr::count(.) %>%
    as.numeric()

  n_false_pos <- df %>%
    dplyr::filter(analyte == select_analyte) %>%
    dplyr::filter(err_type == "false_pos") %>%
    dplyr::count(.) %>%
    as.numeric()

  n_spikes <- df %>%
    dplyr::filter(analyte == select_analyte) %>%
    dplyr::filter(!is.na(result)) %>%
    dplyr::filter(spike_value > 0) %>%
    dplyr::count(.) %>%
    as.numeric()

  n_err_free <- df %>%
    dplyr::filter(analyte == select_analyte) %>%
    dplyr::filter(!is.na(result)) %>%
    dplyr::filter(err_type == "no_err_cat") %>%
    dplyr::count(.) %>% as.numeric()

  n_o_range <- df %>%
    dplyr::filter(analyte == select_analyte) %>%
    dplyr::filter(!is.na(result)) %>%
    dplyr::filter(err_type == "range") %>%
    dplyr::count(.) %>% as.numeric()

  f_pos <- f_neg <- o_range <-
    structure(list(conf.int = c(NA, NA),
                   estimate = NA, method = NA,
                   conf.level = NA, alternative = NA),
              class = "binCI")

  #if (n_false_pos > 0)
  f_pos <- {
    binGroup::binCI(n = n_false_pos + n_true_neg,
                    y = n_false_pos,
                    conf.level = 0.95,
                    alternative = "two.sided",
                    method = "CP")
  }

  #  if (n_false_neg > 0)
  f_neg <- {
    binGroup::binCI(n = n_spikes, # length(df$sample_ID),
                    y = n_false_neg,
                    conf.level = 0.95,
                    alternative = "two.sided",
                    method = "CP")
  }
  #  if (n_err_free > 0)
  o_range <- {
    binGroup::binCI(n = n_spikes,
                    y = n_o_range,
                    conf.level = 0.95,
                    alternative = "two.sided",
                    method = "CP")
  }

  f_tbl <- data.frame(
    error_type = c("false postive", "false negative", "range"),
    method = c(f_pos$method,
               f_neg$method,
               o_range$method),
    conf_level = c(f_pos$conf.level,
                   f_neg$conf.level,
                   o_range$conf.level),
    alternative = c(f_pos$alternative,
                    f_neg$alternative,
                    o_range$alternative),
    n_cases = c(n_false_pos,
                n_false_neg,
                n_o_range),
    n_total = c(n_false_pos + n_true_neg,
                n_spikes, n_spikes),
    cl_min = c(f_pos$conf.int[1],
               f_neg$conf.int[1],
               o_range$conf.int[1]),
    cl_mean = c(f_pos$estimate,
                f_neg$estimate,
                o_range$estimate),
    cl_max = c(f_pos$conf.int[2],
               f_neg$conf.int[2],
               o_range$conf.int[2]))

  gt::gt(f_tbl) %>%
    gt::tab_header(paste0("error rates for ",
                          select_analyte)) %>%
    gt::fmt_number(columns = c(6:8), n_sigfig = 3)

}




