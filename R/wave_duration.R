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
#' is then calculated. The waveform type 'pd' independent of any splitting
#' into subforms is calculated at the end. As a result, the pd calculations
#' will appear all at the end of the table. The subforms, pd1 and pd2,
#' will appear in sequence. Duration is reported in seconds.
#'
#' @return A tibble object containing a row per waveform instance and two
#' columns, waveform and duration, is returned.
#' @export
#'
#' @family waveform functions



wave_duration <- function(data) {

  waveform = wave_group = time = duration = NULL
  rm(list = c("waveform", "wave_group", "time", "duration"))

  out = data %>%
    # wave_group is each waveform/na period
    dplyr::mutate(wave_group = rep(1:length(rle(waveform)[[1]]),
                                   rle(waveform)[[1]])) %>%
    # remove na periods
    dplyr::filter(!is.na(waveform)) %>%
    # put wave_group in numeric order
    dplyr::mutate(wave_group = rep(1:length(rle(wave_group)[[1]]),
                                   rle(wave_group)[[1]])) %>%
    # find duration of each wave_group in seconds
    dplyr::group_by(wave_group) %>%
    dplyr::summarise(waveform = waveform[1],
                     duration = round(max(time) - min(time), 2),
                     .groups = "drop") %>%
    dplyr::filter(waveform != "pd") %>%
    dplyr::select(waveform, duration)

  pdonly <- pd_helper(data) %>%
    dplyr::summarise(waveform = waveform[1],
                     duration = round(max(time) - min(time), 2),
                     .groups = "drop") %>%
    dplyr::filter(waveform == "pd") %>%
    dplyr::select(waveform, duration)

  out <- rbind(out, pdonly)
  return(out)
}
