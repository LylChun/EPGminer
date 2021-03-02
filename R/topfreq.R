#' Calculate the single main frequency for a voltage time series
#'
#' @description The function topfreq calculates the single main frequency
#' for voltage time series data using the Fast Fourier Transform.
#'
#' @usage topfreq(data, window = TRUE, filter = 0.25, pre_fft = FALSE)
#'
#' @inheritParams mainfreqs
#'
#' @details The main frequency is defined as that which has the largest amplitude.
#'
#' @return
#' The single main frequency and its amplitude is returned as a tibble object.
#'
#' @export
#'
#' @family frequency related functions


topfreq <- function (data, window = TRUE, filter = 0.25,
                     pre_fft = FALSE) {

  mainfreq = NULL
  rm(list = c("mainfreq"))

  freqs <- mainfreqs(data, window = window, filter = filter,
                     pre_fft = pre_fft)
  top <- freqs %>%
    # remove result of high pass filter - specific to 1024 window size
    dplyr::filter(round(mainfreq, 2) != 0.29) %>%
    # pick main
    dplyr::slice(1)

  return(top)
}
