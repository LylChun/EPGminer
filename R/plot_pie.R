#' Pie chart of labelled EPG data
#'
#' @description The function plot_pie allows one to visualize EPG waveforms in a
#' pie chart format.
#'
#' @usage plot_pie(data, pietype = c("time", "count"),
#' waveforms = c("A", "C", "E1", "E2", "G", "pd1", "pd2", "pd"))
#'
#' @inheritParams plot_wave
#' @inheritParams plot_fbox
#' @param pietype Select the type of pie chart to display. Default is time - a pie
#' chart showing the percent of time spent in each waveform is displayed. If "count"
#' is selected, then the number of instances for each waveform type is displayed - e.g.
#' if there are 20 distince waveforms, and 5 are of type C, then C will be 5/20 or
#' 25%.
#'
#' @details The function plot_pie is designed to help visualize EPG waveform data.
#'
#' @return A plotly pie chart of EPG waveforms in a dataset.
#' @export
#'
#' @family waveform functions
#'

plot_pie <- function (data, pietype = c("time", "count"),
                      waveforms = c("A", "C", "E1", "E2", "G", "pd1", "pd2", "pd")) {

  waveform = duration = NULL
  rm(list = c("waveform", "duration"))

  pietype = match.arg(pietype)

  if (pietype == "time") {

    plot_data <- wave_duration(data) %>%
      dplyr::group_by(waveform) %>%
      # duration is in seconds, divide by 60 to get minutes
      dplyr::summarise(waveform = waveform[1], time = round(sum(duration)/60, 2),
                .groups = "drop") %>%
      dplyr::filter(waveform %in% waveforms)

    plotly::plot_ly(plot_data, labels = ~waveform, values = ~time, type = 'pie',
                    textinfo = 'percent',
                    hoverinfo = 'text+label',
                    text = ~paste(time, "minutes"))
  }

  else if (pietype == "count") {
    plot_data <- wave_count(data) %>%
      dplyr::filter(waveform %in% waveforms)

    plotly::plot_ly(plot_data, labels = ~waveform, values = ~count, type = 'pie',
                    textposition = 'inside',
                    textinfo = 'percent',
                    hoverinfo = 'text+label',
                    text = ~paste(count))
  }


}
