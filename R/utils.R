seqvec <- Vectorize(seq.default, vectorize.args = c("from", "to"))

smooth_vts <- function (data, interval = 127) {
  out = tibble::tibble(time = data$time, volts = stats::runmed(data$volts, interval))
  return(out)
}

pd_helper <- function (data) {

  waveform = wave_group = NULL
  rm(list = c("waveform", "wave_group"))

  out <- data %>%
    dplyr::filter(waveform %in% c("pd1", "pd2")) %>%
    dplyr::mutate(waveform = dplyr::if_else(waveform == "pd1", "pdb1", "pdb2")) %>%
    dplyr::mutate(wave_group = rep(1:length(rle(waveform)[[1]]),
                                   rle(waveform)[[1]]))

  # returns tibble with only pdb1/pdb2 - time, volts, waveform, wave_group but ungrouped
  return(out)
}
