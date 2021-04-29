#' Calculate voltage metrics for each waveform
#'
#' @description The function wave_volts calculates the voltage metrics
#' for each waveform instance
#'
#' @usage wave_volts(data)
#'
#' @inheritParams plot_wave
#'
#' @details Labeled data is grouped by each waveform instance and the following three
#' voltage metrics are returned: median, SD, and amplitude.
#' The amplitude is defined as the range of voltage values while the SD is the
#' mathematical SD. Median is first corrected for by subtracting the starting
#' voltage, then the median relative (adjusted) voltage is returned.
#'
#' @return A tibble object containing a row per waveform instance and four
#' columns is returned: waveform, median_volts, sd_volts, amplitude_volts.
#' @export
#'
#' @family waveform functions

wave_volts <- function(data) {

  waveform = wave_group = time = median_volts = sd_volts =
    amplitude_volts = pd = volts = NULL
  rm(list = c("waveform", "wave_group", "time", "median_volts", "sd_volts",
              "amplitude_volts", "pd", "volts"))

  udat <- data %>%
    dplyr::mutate(pd = dplyr::if_else(waveform %in% c("pd", "pd1", "pd2"), "pd", waveform)) %>%
    dplyr::mutate(wave_group = rep(1:length(rle(pd)[[1]]),
                                   rle(pd)[[1]])) %>%
    dplyr::group_by(wave_group) %>%
    dplyr::mutate(waveform = dplyr::case_when(
      pd == "pd" & any(waveform %in% c("pd1", "pd2")) ~ "pdb",
      pd == "pd" & all(waveform == "pd") ~ "pda",
      TRUE ~ waveform
    )) %>%
    dplyr::ungroup() %>%
    dplyr::select(time, volts, waveform)

  if (any(unique(udat$waveform == "non-probing"))) {
    begin = stats::median(wave_extract(udat, wave = "non-probing")[[1]]$volts)
  }
  else if (any(is.na(waveform))) {
    begin = stats::median(udat$volts[is.na(udat$waveform)][1:1000])
  }

  out = udat %>%
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
                     # median is relative to baseline begin
                     median_volts = stats::median((volts - begin), na.rm = TRUE),
                     sd_volts = stats::sd(volts, na.rm = TRUE),
                     amplitude_volts = max(volts) - min(volts),
                     .groups = "drop") %>%
    dplyr::select(-wave_group)

  pdsubforms <- pd_helper(data) %>%
    dplyr::group_by(wave_group) %>%
    dplyr::summarise(waveform = waveform[1],
                     median_volts = stats::median(volts, na.rm = TRUE),
                     sd_volts = stats::sd(volts, na.rm = TRUE),
                     amplitude_volts = max(volts) - min(volts),
                     .groups = "drop") %>%
    dplyr::select(-wave_group)

  out <- rbind(out, pdsubforms)
  return(out)
}
