#' Calculate and Extract the number of occurrences for each waveform type
#'
#' @description The function wave_occurrence calculates the number of each waveform
#' type.
#'
#' @usage wave_occurrence(data)
#'
#' @inheritParams plot_wave
#'
#' @details Labeled data is grouped by each waveform instance and the
#' unique instances of each waveform type are counted.
#'
#' @return A tibble object containing a row per waveform instance and two
#' columns, waveform and occurrence, is returned.
#'
#' @export
#'
#' @family waveform functions

wave_occurrence <- function(data) {

  waveform = wave_group = time = pd = volts = NULL
  rm(list = c("waveform", "wave_group", "time", "pd", "volts"))

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

  out = data %>%
    # wave_group is each waveform/na period
    dplyr::mutate(wave_group = rep(1:length(rle(waveform)[[1]]),
                                   rle(waveform)[[1]])) %>%
    dplyr::group_by(waveform) %>%
    dplyr::summarise(waveform = waveform[1], occurrence = length(unique(wave_group)),
                     .groups = "drop") %>%
    # account for pd separately (still keep occurrences for pd1/2)
    dplyr::filter(waveform != "pd")

  pdonly <- pd_helper(data) %>%
    dplyr::filter(waveform == "pd")
  dplyr::ungroup() %>%
    dplyr::summarise(waveform = waveform[1], count = length(unique(wave_group)),
                     .groups = "drop")

  # check for phloem feeding, if not present assign number 0
  if (!any(unique(out$waveform) == "E1")) {
    out <- out %>%
      dplyr::add_row(waveform = "E1", occurrence = 0)
  }
  if (!any(unique(out$waveform) == "E2")) {
    out <- out %>%
      dplyr::add_row(waveform = "E2", occurrence = 0)
  }

  #### pda/b split notation - deprecated
  # pdsubforms <- pd_helper(data) %>%
  #   dplyr::group_by(waveform) %>%
  #   dplyr::summarise(waveform = waveform[1],
  #                    occurrence = length(unique(wave_group)),
  #                    .groups = "drop")

  out <- rbind(out, pdonly)
  return(out)
}
