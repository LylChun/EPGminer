#' Calculate the Number of each waveform
#'
#' @description The function wave_number calculates the number of each waveform
#' type.
#'
#' @usage wave_number(data)
#'
#' @inheritParams plot_wave
#'
#' @details Labelled data is grouped by each waveform instance and the
#' unique instances of each waveform type are counted. The waveform type
#' 'pd' independent of any splitting into subforms is calculated at the end.
#' As a result, the pd calculations  will appear all at the end of the table.
#' The subforms, pd1 and pd2, will appear in sequence.
#'
#' @return A tibble object containing a row per waveform instance and two
#' columns, waveform and number, is returned.
#'
#' @export
#'
#' @family waveform functions

wave_number <- function(data) {

  waveform = wave_group = time = NULL
  rm(list = c("waveform", "wave_group", "time"))

  out = data %>%
    dplyr::mutate(wave_group = rep(1:length(rle(waveform)[[1]]),
                                   rle(waveform)[[1]])) %>%
    dplyr::group_by(waveform) %>%
    dplyr::summarise(waveform = waveform[1], number = length(unique(wave_group)),
                     .groups = "drop") %>%
    dplyr::filter(waveform != "pd")

  pdonly <- pd_helper(data) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(waveform) %>%
    dplyr::summarise(waveform = waveform[1], number = length(unique(wave_group)),
                     .groups = "drop") %>%
    dplyr::filter(waveform == "pd")

  out <- rbind(out, pdonly)
  return(out)
}
