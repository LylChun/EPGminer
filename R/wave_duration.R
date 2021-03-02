#' Calculate and Extract the duration for each waveform instance
#'
#' @description The function wave_duration calculates the duration for
#' each waveform instance in labelled data.
#'
#' @usage wave_duration(data)
#'
#' @inheritParams plot_wave
#'
#' @details Labelled data is grouped by each waveform instance and the duration
#' is then calculated.
#'
#' @return A tibble object containing a row per waveform instance and two
#' columns, waveform and duration, is returned.
#' @export
#'
#' @family waveform functions



wave_duration <- function(data) {

  waveform = wave_group = time = NULL
  rm(list = c("waveform", "wave_group", "time"))

  out = data %>%
    # wave_group is each waveform/na period
    dplyr::mutate(wave_group = rep(1:length(rle(waveform)[[1]]),
                                   rle(waveform)[[1]])) %>%
    # remove na periods
    dplyr::filter(!is.na(waveform)) %>%
    # put wave_group in numeric order
    dplyr::mutate(wave_group = rep(1:length(rle(wave_group)[[1]]),
                                   rle(wave_group)[[1]])) %>%
    # find duration of each wave_group
    dplyr::group_by(wave_group) %>%
    dplyr::summarise(waveform = waveform[1],
                     duration = max(time) - min(time))

  return(out)
}
