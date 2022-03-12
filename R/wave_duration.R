#' Calculate the duration for each waveform instance
#'
#' @description The function wave_duration calculates the duration for
#' each waveform instance in labeled data.
#'
#' @usage wave_duration(data)
#'
#' @inheritParams plot_wave
#'
#' @details Labeled data is grouped by each waveform instance and the duration
#' is then calculated. The overall feeding duration is displayed in the top row
#' as "Feeding" for simplicity. Duration is reported in seconds.
#'
#' The waveform type 'pd' without any splitting into subforms is calculated separately.
#' As a result, the pd calculations will appear all at the end of the table.
#' The subforms, pd1 and pd2, will appear in sequence.
#'
#' @return A tibble object containing a row per waveform instance and two
#' columns, waveform and duration, is returned.
#' @export
#'
#' @family waveform functions



wave_duration <- function(data) {

  waveform = wave_group = time = duration = pd =
    volts = NULL
  rm(list = c("waveform", "wave_group", "time", "duration", "pd", "volts"))

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
                     duration = round(max(time) - min(time), 2),
                     .groups = "drop") %>%
    # pds will be handled separately
    dplyr::filter(waveform != "pd") %>%
    dplyr::select(-wave_group)

  # only calculate pds if they exist in the data
  if (any(data$waveform %in% c("pd", "pd1", "pd2"))) {
    # aggregate pds (pd_helper returns grouped tibble with pd or non as waveforms)
    pdonly <- pd_helper(data) %>%
      dplyr::filter(waveform == "pd") %>%
      dplyr::summarise(waveform = waveform[1],
                       duration = round(max(time) - min(time), 2),
                       .groups = "drop") %>%
      dplyr::select(-wave_group)

    out <- rbind(out, pdonly)
  }
  # check for phloem feeding, if false - duration = 0
  if (!any(unique(out$waveform) == "E1")) {
    out <- out %>%
      dplyr::add_row(waveform = "E1", duration = 0)
  }
  if (!any(unique(out$waveform) == "E2")) {
    out <- out %>%
      dplyr::add_row(waveform = "E2", duration = 0)
  }


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

  #### pda/b split notation - deprecated
  # pdsubforms <- pd_helper(data) %>%
  #   dplyr::group_by(wave_group) %>%
  #   dplyr::summarise(waveform = waveform[1],
  #                    duration = round(max(time) - min(time), 2),
  #                    .groups = "drop") %>%
  #   dplyr::select(-wave_group)

  total <- tibble::tibble(
    waveform = "Feeding",
    # waveform pd includes all of pd, pd1, pd2 (from rbinding pdonly to out)
    duration = sum(out$duration[out$waveform %in% c("A", "C", "G", "E1", "E2", "pd")])
  )

  out <- dplyr::bind_rows(total, out)
  return(out)
}
