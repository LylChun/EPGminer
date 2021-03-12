#' Bar Chart of Frequencies
#'
#' @description The function plot_fbar shows the frequencies present in a dataset
#' plotted in a bar chart by waveform.
#'
#' @usage plot_fbar(data, waveforms = c("A", "C", "E1", "E2", "G", "pd1",
#' "pd2", "pd"))
#'
#' @inheritParams plot_wave
#' @param waveforms A list of waveforms to include in the plot. Default is all.
#'
#' @details The function plot_fbar is designed to allow one to see the frequencies
#' and the variability of frequencies for waveforms in a dataset.
#'
#' @return A bar chart of frequencies by waveform
#' @export
#'
#' @family frequency related functions
#'
#'

plot_fbar <- function (data, waveforms = c("A", "C", "E1", "E2", "G", "pd1",
                                           "pd2", "pd")) {
  out <- wave_topfreq(data)
  colnames(out) <- c("group", "waveform", "frequency")
  out <- out[, 2:3] %>%
    # filter after finding top frequency to avoid run-on errors
    dplyr::filter(waveform %in% waveforms)

  ggplot2::ggplot(out) +
    ggplot2::geom_boxplot(ggplot2::aes(waveform, frequency)) +
    ggplot2::theme_minimal()
}
