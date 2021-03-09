#' Calculate and Extract the top frequency for each waveform instance
#'
#' @description The function wave_topfreq calculates the main/top frequency for
#' each waveform instance in labelled data.
#'
#' @usage wave_topfreq(data)
#'
#' @inheritParams plot_wave
#'
#' @details Labelled data is grouped by each waveform instance and the
#' Fourier transform is then used to extract the top frequency via the
#' epgminer function topfreq.
#'
#' @return A tibble object containing a row per waveform instance and two
#' columns, waveform and frequency, is returned.
#' @export
#'
#' @family frequency related functions
#' @family waveform functions

wave_topfreq <- function (data) {

  waveform = wave_group = suffish = time = volts = NULL
  rm(list = c("waveform", "wave_group", "suffish", "time", "volts"))

  out = data %>%
    # wave_group is each waveform/na period
    dplyr::mutate(wave_group = rep(1:length(rle(waveform)[[1]]),
                                   rle(waveform)[[1]])) %>%
    # remove na periods
    dplyr::filter(!is.na(waveform)) %>%
    # for each group determine data sufficiency
    dplyr::group_by(wave_group) %>%
    # make sure enough data for topfreq (Nyquist 50)
    # may not actually be working as I thought it does
    dplyr::mutate(suffish = ifelse(length(wave_group) >= 50, TRUE, FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::slice(which(suffish)) %>%
    # put wave_group in numeric order
    dplyr::mutate(wave_group = rep(1:length(rle(wave_group)[[1]]),
                                   rle(wave_group)[[1]])) %>%
    dplyr::group_by(wave_group) %>%
    dplyr::summarise(waveform = waveform[1],
                     topfreq = topfreq(data.frame(time, volts))$mainfreq,
                     .groups = "drop")

  return(out)
}
