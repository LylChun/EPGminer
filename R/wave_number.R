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

  waveform = wave_group = time = pd = volts = NULL
  rm(list = c("waveform", "wave_group", "time", "pd", "volts"))

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

  out = udat %>%
    # wave_group is each waveform/na period
    dplyr::mutate(wave_group = rep(1:length(rle(waveform)[[1]]),
                                   rle(waveform)[[1]])) %>%
    dplyr::group_by(waveform) %>%
    dplyr::summarise(waveform = waveform[1], number = length(unique(wave_group)),
                     .groups = "drop")

  # check for phloem feeding, if not present assign number 0
  if (!any(unique(out$waveform) == "E1")) {
    out <- out %>%
      dplyr::add_row(waveform = "E1", number = 0)
  }
  if (!any(unique(out$waveform) == "E2")) {
    out <- out %>%
      dplyr::add_row(waveform = "E2", number = 0)
  }

  pdsubforms <- pd_helper(data) %>%
    dplyr::group_by(waveform) %>%
    dplyr::summarise(waveform = waveform[1],
                     number = length(unique(wave_group)),
                     .groups = "drop")

  out <- rbind(out, pdsubforms)
  return(out)
}
