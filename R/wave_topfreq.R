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
#' epgminer function topfreq. The waveform type 'pd' independent of any
#' splitting into subforms is calculated at the end. As a result, the
#' pd calculations will appear all at the end of the table. The subforms,
#' pd1 and pd2, will appear in sequence. Frequency is reported in Hz.
#'
#' @return A tibble object containing a row per waveform instance and two
#' columns, waveform and frequency, is returned.
#' @export
#'
#' @family frequency related functions
#' @family waveform functions

wave_topfreq <- function (data) {

  waveform = wave_group = suffish = time = volts = frequency = NULL
  rm(list = c("waveform", "wave_group", "suffish", "time", "volts", "frequency"))

  out = data %>%
    # wave_group is each waveform/na period
    dplyr::mutate(wave_group = rep(1:length(rle(waveform)[[1]]),
                                   rle(waveform)[[1]])) %>%
    # remove na periods
    dplyr::filter(!is.na(waveform)) %>%
    # put wave_group in numeric order
    dplyr::mutate(wave_group = rep(1:length(rle(wave_group)[[1]]),
                                   rle(wave_group)[[1]])) %>%
    dplyr::group_by(wave_group) %>%
    dplyr::summarise(waveform = waveform[1],
                     frequency = round(topfreq(data.frame(time, volts))$mainfreq, 2),
                     .groups = "drop") %>%
    dplyr::filter(waveform != "pd") %>%
    dplyr::select(waveform, frequency)

  pdonly <- pd_helper(data) %>%
    dplyr::summarise(waveform = waveform[1],
                     frequency = round(topfreq(data.frame(time, volts))$mainfreq, 2),
                     .groups = "drop") %>%
    dplyr::filter(waveform == "pd") %>%
    dplyr::select(waveform, frequency)

  out <- rbind(out, pdonly)
  return(out)
}
