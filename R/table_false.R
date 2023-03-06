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
#' @param dat data frame with all data needed as described in `get_data`.
#'
#' @examples
#' example_spike_data <- system.file('extdata', 'spikevals.csv', package = 'blindspiker')
#' example_lab_data <- system.file('extdata', 'labvals.csv', package = 'blindspiker')
#' example_df <- get_data(spike_data = example_spike_data, lab_data = example_lab_data)
#' table_false(select_analyte = "Sr-90", dat = example_df)
#'
#' @export
#'
table_false <- function(select_analyte,
                        dat = bs_df) {

  # To avoid visible binding note in package check:
  result <- err_type <- det_lvl <- analyte <- bs_df <- `.` <- NULL

  my_analytes <- unique(dat$analyte)

  stopifnot("Argument, select_analyte, is not in the data set." =
            select_analyte %in% my_analytes)

  df <- dat %>% dplyr::filter(analyte == select_analyte)

  # false postive
  # The false positive rate is computed as the number of false positives divided
  # by the number of false positives and true negatives. True negatives are
  # computed first

    n_true_neg <- df %>%
    dplyr::filter(analyte == select_analyte) %>%
    dplyr::filter(err_type == "no_err_cat") %>%
    dplyr::filter(result < det_lvl) %>%
    dplyr::count(.) %>%
    as.numeric()  #

    n_false_neg <- df %>%
      dplyr::filter(analyte == select_analyte) %>%
      dplyr::filter(err_type == "false_neg") %>%
      dplyr::count(.) %>%
      as.numeric()  #

      n_false_pos <-  df %>%
      dplyr::filter(analyte == select_analyte) %>%
      dplyr::filter(err_type == "false_pos") %>%
      dplyr::count(.) %>%
      as.numeric()

      n_lab_results <- df %>%
      dplyr::filter(analyte == select_analyte) %>%
      dplyr::filter(!is.na(result)) %>%
      dplyr::count(.) %>%
      as.numeric()

      n_err_free <-  df %>%
        dplyr::filter(analyte == select_analyte) %>%
        dplyr::filter(!is.na(result)) %>%
        dplyr::filter(err_type == "no_err_cat") %>%
        dplyr::count(.) %>%
        as.numeric()

      # Provide default NA's for f_pos, f_neg, and f_all
      # This was dput from example binCI, and NA's inserted
  f_pos <- f_neg <- f_all <- structure(list(conf.int =
                                    c(NA, NA),
                                    estimate = NA,
                                    method = NA,
                                    conf.level = NA,
                                    alternative = NA),
                                    class = "binCI")

      # false positive rate
  f_pos <- if(n_false_pos > 0) {
    binGroup::binCI(
    n = n_false_pos + n_true_neg,
    y = n_false_pos,
    conf.level = 0.95,
    alternative = "two.sided",
    method = "CP")
    }

    # false negative rate
  f_neg <- if(n_false_neg > 0){
    binGroup::binCI(
      n = length(df$sample_ID),
      y = n_false_neg,
      conf.level = 0.95,
      alternative = "two.sided",
      method = "CP")
    }

  # total error rate
  f_all <- if(n_err_free > 0){
    binGroup::binCI(n = n_lab_results,
                           y = n_lab_results - n_err_free, #either error
                           conf.level = 0.95,
                           alternative = "two.sided",
                           method = "CP")
  }

  # Resume editing here
  # Need to figure out a way to fill table with NA's

f_tbl <- data.frame("error_type" =
                  c("false postive", "false negative", "either_error"),
                  "n" = c(n_false_pos, n_false_neg, n_false_neg + n_false_pos),
                  "min" =
                  c(f_pos$conf.int[1], f_neg$conf.int[1], f_all$conf.int[1]),
                  "mean" =
                  c(f_pos$estimate, f_neg$estimate, f_all$estimate),
                  "max" =
                  c(f_pos$conf.int[2], f_neg$conf.int[2], f_all$conf.int[2]),
                  "method" =
                  c(f_pos$method, f_neg$method, f_all$method),
                  "conf_level" =
                  c(f_pos$conf.level, f_neg$conf.level, f_all$conf.level),
                  "alternative" = c(f_pos$alternative, f_neg$alternative, f_all$alternative))

  gt::gt(f_tbl) %>% gt::tab_header(paste0("error rates for ",
                                            select_analyte)) %>%
  gt::fmt_number(columns = c(3:5), n_sigfig = 3)
}
