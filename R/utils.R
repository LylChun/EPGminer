seqvec <- Vectorize(seq.default, vectorize.args = c("from", "to"))

smooth_vts <- function (data, interval = 127) {
  out = tibble::tibble(time = data$time, volts = stats::runmed(data$volts, interval))
  return(out)
}

pd_helper <- function (data) {

  waveform = wave_group = NULL
  rm(list = c("waveform", "wave_group"))

  out <- data %>%
    dplyr::mutate(waveform = dplyr::if_else(waveform %in% c("pd", "pd1", "pd2"),
                                            "pd", "non")) %>%
    dplyr::mutate(wave_group = rep(1:length(rle(waveform)[[1]]),
                                   rle(waveform)[[1]])) %>%
    dplyr::group_by(wave_group)

  # returns grouped tibble with pd and non, grouped into wave_groups
  return(out)
}
