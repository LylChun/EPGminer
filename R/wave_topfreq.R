#' Calculate and Extract the top frequency for each waveform instance
#'
#' @description The function wave_topfreq calculates the main/top frequency for
#' each waveform instance in labeled data.
#'
#' @usage wave_topfreq(data)
#'
#' @inheritParams plot_wave
#'
#' @details Labeled data is grouped by each waveform instance and the
#' Fourier transform is then used to extract the top frequency via the
#' epgminer function topfreq. Frequency is reported in Hz.
#'
#' The waveform type 'pd' without any splitting into subforms is calculated separately.
#' As a result, the pd calculations will appear all at the end of the table.
#' The subforms, pd1 and pd2, will appear in sequence.
#'
#' @return A tibble object containing a row per waveform instance and two
#' columns, waveform and frequency, is returned.
#' @export
#'
#' @family frequency related functions
#' @family waveform functions

wave_topfreq <- function (data) {

  waveform = wave_group = suffish = time = volts = frequency =
    pd = NULL
  rm(list = c("waveform", "wave_group", "suffish", "time", "volts", "frequency",
              "pd"))

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
    dplyr::select(-wave_group)

  # aggregate pds (pd_helper returns grouped tibble with pd or non as waveforms)
  pdonly <- pd_helper(data) %>%
    dplyr::filter(waveform == "pd") %>%
    dplyr::summarise(waveform = waveform[1],
                     frequency = round(topfreq(data.frame(time, volts))$mainfreq, 2),
                     .groups = "drop") %>%
    dplyr::select(waveform, frequency)

  out <- rbind(out, pdonly)


  ####### deprecated pda/b notation
  ### pdb1/2 notation - see note on pda/pdb split
  # pdsubforms <- pd_helper(data) %>%
  #   dplyr::group_by(wave_group) %>%
  #   dplyr::summarise(waveform = waveform[1],
  #                    frequency = round(topfreq(data.frame(time, volts))$mainfreq, 2),
  #                    .groups = "drop") %>%
  #   dplyr::select(-wave_group)

  ### pda/b notation - not found to be significant difference, left for completeness
  # udat <- data %>%
  #   dplyr::mutate(pd = dplyr::if_else(waveform %in% c("pd", "pd1", "pd2"), "pd", waveform)) %>%
  #   dplyr::mutate(wave_group = rep(1:length(rle(pd)[[1]]), rle(pd)[[1]])) %>%
  #   dplyr::group_by(wave_group) %>%
  #   dplyr::mutate(waveform = dplyr::case_when(
  #     pd == "pd" & any(waveform %in% c("pd1", "pd2")) ~ "pdb",
  #     pd == "pd" & all(waveform == "pd") ~ "pda",
  #     TRUE ~ waveform
  #   )) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::select(time, volts, waveform)

  return(out)
}
