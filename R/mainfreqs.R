#' Find the main frequencies in a sample using the Fast Fourier Transform
#'
#' @description The function mainfreqs finds the main frequencies in voltage time-
#' series data by using the Fast Fourier Transform.
#'
#' @usage mainfreqs(data, end = sample_rate/2, pre_fft = FALSE,
#' window = TRUE, filter = 0.25, sample_rate = 100)
#'
#'
#' @inheritParams single_fft
#' @param data Either a voltage time series dataframe or a dataframe of Fourier
#' transformed data with columns for frequency and amplitude. If the latter,
#' change pre_fft to TRUE.
#' @param end Numeric indicating number of frequencies to display, e.g. end = 10
#' will be top 10 frequencies. Default is all (half sample_rate)
#' @param pre_fft TRUE/FALSE indicating whether or not the data being fed has been
#' Fourier Transformed,
#'
#' @details Main frequencies are determined as those having the
#' largest amplitude. Frequencies are aggregated such that only the highest in each
#' integer bin are kept and compared - e.g. if the data is 5.1 Hz, 5.2 Hz, only the
#' largest will be kept for the 5.x Hz entry.
#'
#' @return
#' A dataframe containing the main frequencies with 2 columns is returned:
#' frequency and amplitude.
#'
#' @export
#'
#' @family frequency related functions
#'

mainfreqs <- function (data, end = sample_rate/2, pre_fft = FALSE,
                       window = TRUE, filter = 0.25, sample_rate = 100) {

  mainfreq = frequency = amplitude = NULL
  rm(list = c("mainfreq","frequency", "amplitude"))

  if (!pre_fft) {
    # didn't specify include_dc because will take out later anyways
    data = single_fft(data, window = window, filter = filter,
                      sample_rate = 100)
  }

  taro <- data %>%
    # take out dc component
    dplyr::filter(frequency != 0) %>%
    # cut into Nyquist number of groups, for us is 50 Hz
    dplyr::group_by(ggplot2::cut_number(frequency, n = sample_rate/2, labels = FALSE)) %>%
    # pull main frequencies and amplitudes
    dplyr::summarise(mainfreq = frequency[which.max(amplitude)],
                     amplitude = max(amplitude, na.rm = T)) %>%
    # descending order to see biggest/main freqs
    dplyr::arrange(dplyr::desc(amplitude)) %>%
    dplyr::select(mainfreq, amplitude)

  # end selects how many to output, default is all
  if (end > nrow(taro)) {
    end = nrow(taro)
  }

  out <- taro[1:end, ]
  return(out)
}
