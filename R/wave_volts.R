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
#' voltage metrics are returned: mean, SD, and relative amplitude.
#' The SD is the mathematical SD of volts. Mean is first corrected for by subtracting
#' the starting voltage, then the mean (adjusted) voltage is returned.
#' The waveform type 'pd' without any splitting into subforms is calculated separately.
#' As a result, the pd calculations will appear all at the end of the table.
#' The subforms, pd1 and pd2, will appear in sequence.
#'
#' Relative amplitude is a percent calculated with the following formula:
#' Relative amplitude = |(mean of waveform - mean of nonprobing)|x100/5
#'
#' @return A tibble object containing a row per waveform instance and four
#' columns is returned: waveform, mean_volts, sd_volts, relative_amplitude.
#' @export
#'
#' @family waveform functions

wave_volts <- function(data) {

  waveform = wave_group = time = mean_volts = sd_volts =
    amplitude_volts = pd = volts = NULL
  rm(list = c("waveform", "wave_group", "time", "mean_volts", "sd_volts",
              "amplitude_volts", "pd", "volts"))

  #### pda/b split notation - deprecated
  # udat <- data %>%
  #   dplyr::mutate(pd = dplyr::if_else(waveform %in% c("pd", "pd1", "pd2"), "pd", waveform)) %>%
  #   dplyr::mutate(wave_group = rep(1:length(rle(pd)[[1]]),
  #                                  rle(pd)[[1]])) %>%
  #   dplyr::group_by(wave_group) %>%
  #   dplyr::mutate(waveform = dplyr::case_when(
  #     pd == "pd" & any(waveform %in% c("pd1", "pd2")) ~ "pdb",
  #     pd == "pd" & all(waveform == "pd") ~ "pda",
  #     TRUE ~ waveform
  #   )) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::select(time, volts, waveform)

  if (any(unique(data$waveform == "non-probing"))) {
    begin = mean(wave_extract(data, wave = "non-probing")[[1]]$volts)
  }
  else if (any(is.na(data$waveform))) {
    begin = stats::median(data$volts[is.na(data$waveform)][1:1000])
    # if no non-probing and no na, assume beginning baseline is 0
  } else {begin = 0}

  # find overall sd of feeding data
  # overall_sd <- sd(data$volts[data$waveform != "non-probing"])

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
                     # mean is relative to baseline begin
                     mean_volts = mean((volts - begin), na.rm = TRUE),
                     sd_volts = stats::sd(volts, na.rm = TRUE),
                     # relative amplitude percent (absolute to ensure positive)
                     relative_amplitude = abs(mean(volts) - begin)/5 * 100,
                     .groups = "drop") %>%
    dplyr::filter(waveform != "pd") %>%
    dplyr::select(-wave_group)

  # aggregate pds (pd_helper returns grouped tibble with pd or non as waveforms)
  pdonly <- pd_helper(data) %>%
    dplyr::filter(waveform == "pd") %>%
    dplyr::summarise(waveform = waveform[1],
                     # mean is relative to baseline begin
                     mean_volts = mean((volts - begin), na.rm = TRUE),
                     sd_volts = stats::sd(volts, na.rm = TRUE),
                     # relative amplitude percent (absolute to ensure positive)
                     relative_amplitude = abs(mean(volts) - begin)/5 * 100,
                     .groups = "drop") %>%
    dplyr::select(-wave_group)

  out <- rbind(out, pdonly)

  #### pda/b split notation - deprecated
  # pdsubforms <- pd_helper(data) %>%
  #   dplyr::group_by(wave_group) %>%
  #   dplyr::summarise(waveform = waveform[1],
  #                    mean_volts = mean(volts, na.rm = TRUE),
  #                    sd_volts = stats::sd(volts, na.rm = TRUE),
  #                    relative_amplitude = (begin - mean(volts))/5 * 100,
  #                    .groups = "drop") %>%
  #   dplyr::select(-wave_group)

  return(out)
}
