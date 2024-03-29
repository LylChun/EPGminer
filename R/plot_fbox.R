#' Box plot of Frequencies
#'
#' @description The function plot_fbox shows the frequencies present in a dataset
#' plotted in a box plot by waveform.
#'
#' @usage plot_fbox(data, waveforms = c("non-probing", "C", "E1", "E2", "G", "pd1",
#' "pd2", "pd"))
#'
#' @inheritParams plot_wave
#' @param waveforms A list of waveforms to include in the plot. Default is all.
#'
#' @details The function plot_fbox is designed to allow one to see the frequencies
#' and the variability of frequencies for waveforms in a dataset.
#'
#' @return A boxplot of frequencies by waveform. If none of the specified
#' waveforms are present in the data, then the function will return NULL.
#' @export
#'
#' @family frequency related functions
#'
#'

plot_fbox <- function (data, waveforms = c("non-probing", "C", "E1", "E2", "G",
                                           "pd1", "pd2", "pd")) {

  waveform = frequency = NULL
  rm(list = c("waveform", "frequency"))

  # subset to only user-selected waveforms that are present
  waveforms = intersect(unique(data$waveform), waveforms)
  # if none, then message no plot
  if (length(waveforms) == 0){
    message("None of the selected waveforms are present")
    return(NULL)
  }

  out <- wave_topfreq(data)
  out <- out %>%
    # filter after finding top frequency to avoid run-on errors
    dplyr::filter(waveform %in% waveforms)

  ggplot2::ggplot(out) +
    ggplot2::geom_boxplot(ggplot2::aes(waveform, frequency)) +
    ggplot2::theme_minimal()
}
