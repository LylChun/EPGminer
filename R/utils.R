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


#' Select a single hour out of a larger dataset.
#'
#' @description The function select_hr subsets a user specified hour
#' from a larger dataset.
#'
#' @usage select_hr(data, hour, sample_rate = 100)
#'
#' @inheritParams single_fft
#' @param data May be raw voltage time series data or labelled time series data.
#' @param hour Integer indicating which hour of data to subset - e.g. 2 will select
#' the second hour of data.
#'
#' @details If the selected hour is outside of the data range, or the data has gaps,
#' or the hour is incomplete (as is possible for the last hour), the function may
#' function incorrectly.
#'
#' @return
#' A dataframe subsetting the input data to the specified hour is returned.
#'
#' @export
#'

select_hr <- function (data, hour, sample_rate = 100) {

  len <- sample_rate*60*60
  # get hours since start (if first hour then start is 0)
  # add one to get beginning of next group
  start = (hour-1)*len + 1
  # from start to end overall length 360k
  out = data[start:(start + len - 1), ]

  return(out)
}
