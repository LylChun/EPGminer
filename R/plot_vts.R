#' Time series plot of voltage
#'
#' @description The function plot_vts plots voltage vs time.
#'
#' @usage plot_vts(data, aggregate = TRUE, interval = 100)
#'
#' @inheritParams single_fft
#' @param aggregate TRUE/FALSE indicated whether to aggregate the data and
#' reduce the number of points by plotting only the mean over a set interval.
#' @param interval Integer indicating the number of points to aggregate
#' together if aggregate = TRUE.
#'
#' @details If a large amount of data is fed in, it is recommended to aggregate
#' because plotting an excess of points is time intensive.
#'
#' @return A voltage time series plot.
#' @export
#'


plot_vts <- function (data, aggregate = TRUE, interval = 100) {

  time = win = volts = NULL
  rm(list = c("time", "win", "volts"))

  # to make plotting faster
  if (aggregate) {
    # one sec aggregates
    out = data %>%
      # interval is secs per interval*100 bc 100 Hz
      dplyr::mutate(win = cut(time, breaks = floor(length(time)/interval),
                              labels = FALSE)) %>%
      dplyr::group_by(win) %>%
      dplyr::summarise(time = time[1],
                       volts = mean(volts, na.rm = TRUE))
  } else {out = data}

  ggplot2::ggplot(out) +
    ggplot2::geom_line(ggplot2::aes(time, volts)) +
    ggplot2::ggtitle(label = paste(deparse(substitute(data)),
                                   ": Voltage vs Time", sep = "")) +
    ggplot2::xlab(label = "time (in secs)")
}

