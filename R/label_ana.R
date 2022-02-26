#' Use ANA file to label raw data
#'
#' @description The function label_ana combines raw data and an
#' ANA annotation together to return a labeled dataframe.
#'
#' @usage label_ana(data, ana)
#'
#' @inheritParams single_fft
#' @param ana A dataframe for an ANA file.
#'
#' @details The raw data and ana must be for the same dataset, although the
#' names of the objects do not need to match in any way. The following code
#' number to character title conversion is followed:
#' 1 = non-probing
#' 2 = C
#' 4 = E1
#' 5 = E2
#' 7 = G
#' 8 = pd
#' 9 = pd1
#' 10 = pd2
#' 99 = end
#' If the ANA labels do not come in that exact code matching, then the labeling
#' will be incorrect.
#'
#' @return A tibble object with three rows is returned: time, volts, waveform.
#' @export
#'
#' @seealso \code{\link{read_epg}} to read in the raw data and ANA files.

label_ana <- function (data, ana) {

  waveform = time = idx = NULL
  rm(list = c("waveform","time", "idx"))

  data$time <- round(data$time, 2)
  ana <- ana %>%
    dplyr::filter(waveform != 99) %>%
    dplyr::mutate(time = round(time, 2),
                  waveform = dplyr::case_when(
                    waveform == 1 ~ "non-probing",
                    waveform == 2 ~ "C",
                    waveform == 7 ~ "G",
                    waveform == 8 ~ "pd",
                    waveform == 9 ~ "pd1",
                    waveform == 10 ~ "pd2",
                    waveform == 4 ~ "E1",
                    waveform == 5 ~ "E2",
                    waveform == 99 ~ "end",
                  ))

  # combine raw data with ana labels
  out <- dplyr::left_join(data, ana, by = "time") %>%
    # replace NA for rle
    dplyr::mutate(waveform = ifelse(is.na(waveform), "fill", waveform))
  # define which rows to keep in rle - half length out bc half is fill
  evens <- seq(2, by = 2, length.out = length(rle(out$waveform)[[1]])/2)
  # create idx with 1/2 # categories as waveform bc 1/2 waveform is "fill"
  out <- out %>%
    dplyr::mutate(idx = rep(1:(length(rle(waveform)[[1]])/2),
                            # each is length of fill + 1 for label immediately before it
                            rle(waveform)[[1]][evens] + 1)) %>%
    dplyr::group_by(idx) %>%
    dplyr::mutate(waveform = waveform[1]) %>%
    dplyr::ungroup() %>%
    dplyr::select(-idx)

  return(out)
}
