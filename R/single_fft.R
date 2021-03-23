window_fft <- function (data, filter = 0.25, sample_rate = 100) {

  # define available data for individual waveform
  sample_size = length(data$volts)
  # define n-point fft (1024 = 2^10)
  L = 1024

  # if less than desired length, use all available data
  if (sample_size < L) {
    data = data
    # redefine length to available data
    L = sample_size
  } else {
    # find center of data length
    center = floor(sample_size/2)
    half = L/2
    # centered cut of data
    data = data[(center-half+1):(center+half), ]
  }

  # define frequencies
  freqs = sample_rate*(0:(L/2))/L

  # create Hann window coefficients
  window = hann(L)
  # window the data
  volts = data$volts * window
  # transform the data using fft
  xform = stats::fft(volts)
  # to get amplitude must take the absolute
  # and then scale by length of data - sum(window) bc of Hann
  amp = abs(xform/sum(window))
  # for single sided, first select only to Nyquist
  half = amp[1:((L/2)+1)]
  # then multiply by two, except DC and Nyquist
  half[2:(length(half)-1)] <- 2*half[2:(length(half)-1)]
  # define frequencies
  freqs = sample_rate*(0:(L/2))/L
  out = tibble::tibble(frequency = freqs, amplitude = half) %>%
    # for direct comparison
    dplyr::mutate(amplitude = dplyr::case_when(
      # high pass filter to remove extremely low freq noise
      # remove dc and bleed into dc
      frequency <= filter ~ 0,
      TRUE ~ amplitude
    ))

  return(out)
}

hann <- function (length) {
  n = length - 1
  # 0:n to vectorize through nth number
  out = 0.5*(1 - cos(2*pi*(0:n)/n))
}

#' Return the frequency and amplitude using the Fast Fourier Transform
#'
#' @description The function, single_fft, runs the R implementation of the
#' Fast Fourier Transform, fft, and returns a dataframe containing the single-
#' sided FFT with columns for frequency and amplitude.
#'
#' @usage single_fft(data, window = TRUE, include_dc = TRUE, filter = 0.25,
#' sample_rate = 100)
#'
#' @param data Dataframe containing columns for time and volts
#' @param window Default true will window the data before transforming.
#' @param include_dc Default true, will include DC component (amplitude at 0 Hz)
#' @param filter High pass filter indicating threshold frequency above which
#' will be kept, and below which will be filtered out
#' @param sample_rate The frequency of collection in Hz. Default is 100 Hz
#'
#' @details If window = TRUE is chosen, then a Hann (Hanning) window is applied
#' to the data for the transform. Additionally, window = TRUE will select a
#' 2^10 = 1024 point data.frame from the center of the data in order to get
#' a 'center cut' of the data and avoid irregularities around the edges.
#'
#' @return A tibble object with two columns: Frequency and Amplitude. It is
#' single-sided, and will select up to the Nyquist (sample_rate/2)
#'
#' @export
#'
#' @family frequency related functions


single_fft <- function (data, window = TRUE, include_dc = TRUE,
                        filter = 0.25, sample_rate = 100) {

  # cleaning enviro
  L = xform = amp = half = NULL
  rm(list = c("L", "xform", "amp", "half"))

  if (window) {
    out = window_fft(data, filter = filter, sample_rate = sample_rate)
  } else {
    # define the length, used for scaling later on
    L = length(data$volts)
    # transform the data
    xform = stats::fft(data$volts)
    # to get amplitude must take the absolute
    # also scale by length of data
    amp = abs(xform/L)
    # for single sided, first select only to Nyquist
    P1 = amp[1:((L/2)+1)]
    # then multiply by two, except DC and Nyquist
    P1[2:(length(P1)-1)] <- 2*P1[2:(length(P1)-1)]
    # define frequencies
    freqs = sample_rate*(0:(L/2))/L
    out = dplyr::tibble(frequency = freqs, amplitude = P1) %>%
      dplyr::mutate(amplitude = dplyr::case_when(
        # apply filter for consistency (direct comparison)
        frequency <= filter ~ 0,
        TRUE ~ amplitude
      ))

    if(!include_dc) {
      out$amplitude[1] <- 0
    }
  }

  return(out)
}
