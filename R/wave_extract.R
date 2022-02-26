#' Extract a specified waveform from a labeled dataset
#'
#' @description The function wave_extract subsets data and returns the specified
#' waveform.
#'
#' @usage wave_extract(data, wave, number = 1)
#'
#' @inheritParams plot_wave
#' @param wave Desired type of waveform, e.g. pd1
#' @param number Integer denoting which instance of the specified waveform type to
#' select - e.g. number = 1 will select the first instance of waveform type specified
#' by wave parameter.
#'
#' @details If the wave parameter input does not exactly match a waveform label
#' in the data, the function will not work.
#'
#' @return A list containing 2 entries is returned: entry one is a tibble of
#' time, volts, waveform is returned containing just the specified
#' waveform instance. List entry 2 is the number/count of that type of waveform -
#' e.g. if wave = "pd2" and there are 12 pd2 instances in the dataset, this
#' entry will return 12.
#'
#' @export
#'
#' @family waveform functions

wave_extract <- function (data, wave, number = 1) {

  waveform = wave_group = NULL
  rm(list = c("waveform", "wave_group"))

  udat = data %>%
    # wave_group is each waveform/na period
    dplyr::mutate(wave_group = rep(1:length(rle(waveform)[[1]]),
                            rle(waveform)[[1]])) %>%
    dplyr::filter(waveform == wave) %>%
    # wave_group now is in numeric order with only desired waveform type
    dplyr::mutate(wave_group = rep(1:length(rle(wave_group)[[1]]),
                            rle(wave_group)[[1]]))

  out <- udat %>%
    # select which waveform (eg. first pd, second, etc)
    # using in so can select more than one, should still have
    # wave_group designation to separate
    dplyr::filter(wave_group %in% number)

  numevents = length(unique(udat$wave_group))

  return(list(out, numevents))
}
