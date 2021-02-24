#' Plot of amplitude vs frequency for transformed voltage data
#'
#' @description The function plot_fft allows one to visualize the results
#' of a Fourier Transform and check for peaks - main constituent frequencies.
#'
#' @usage plot_fft(data, end = NULL, include_dc = FALSE, pre_fft = FALSE,
#' window = TRUE, filter = 0.25, sample_rate = 100)
#'
#' @inheritParams mainfreqs
#' @inheritParams single_fft
#' @param end Default is NULL to plot all data. Otherwise, insert numeric value
#' corresponding to the maximum frequency you would like to plot. This does
#' not affect the frequency values themselves, it merely selects the portion
#' of the graph to view.
#'
#' @details If window = TRUE is chosen, then a Hann (Hanning) window is applied
#' to the data for the transform. Additionally, window = TRUE will select a
#' 2^10 = 1024 point data.frame from the center of the data in order to get
#' a 'center cut' of the data and avoid irregularities around the edges.
#'
#' @return A plot showing amplitude vs frequency of the Fourier Transformed data.
#' @export
#'


plot_fft <- function (data, end = NULL, include_dc = FALSE, pre_fft = FALSE,
                      window = TRUE, filter = 0.25, sample_rate = 100) {
  out = frequency = amplitude = NULL
  rm(list = c("out", "frequency", "amplitude"))

  if (!pre_fft) {
    out = single_fft(data, include_dc = include_dc, window = window,
                     filter = filter, sample_rate = sample_rate)
  } else {out = data}

  # to set limit of window to defined frequency
  if (!is.null(end)) {
    end = which(round(out$frequency) == end)[1]
    out = out[1:end, ]
  }

  ggplot2::ggplot(out) +
    ggplot2::geom_line(ggplot2::aes(frequency, amplitude)) +
    ggplot2::ggtitle(label = paste(deparse(substitute(data)),
                                   ": FFT Amplitude vs Frequency",
                                   sep = ""))
}
