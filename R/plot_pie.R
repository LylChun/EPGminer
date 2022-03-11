#' Pie chart of labeled EPG data
#'
#' @description The function plot_pie allows one to visualize EPG waveforms in a
#' pie chart format.
#'
#' @usage plot_pie(data, pietype = c("time", "number"),
#' waveforms = c("A", "C", "E1", "E2", "G", "pd1", "pd2", "pd"))
#'
#' @inheritParams plot_wave
#' @inheritParams plot_fbox
#' @param pietype Select the type of pie chart to display. Default is time - a pie
#' chart showing the percent of time spent in each waveform is displayed. If "number"
#' is selected, then the number of instances for each waveform type is displayed - e.g.
#' if there are 20 distinct waveforms, and 5 are of type C, then C will be 5/20 or
#' 25%.
#'
#' @details The function plot_pie is designed to help visualize EPG waveform data.
#'
#' @return A plotly pie chart of EPG waveforms in a dataset. If none of the specified
#' waveforms are present in the data, then the function will return NULL.
#' @export
#'
#' @family waveform functions
#'

plot_pie <- function (data, pietype = c("time", "number"),
                      waveforms = c("non-probing", "C", "E1", "E2", "G", "pd1",
                                    "pd2", "pd")) {

  waveform = duration = NULL
  rm(list = c("waveform", "duration"))

  pietype = match.arg(pietype)

  # subset to only user-selected waveforms that are present
  waveforms = intersect(unique(data$waveform), waveforms)
  # if none, then message no plot
  if (length(waveforms) == 0){
    message("None of the selected waveforms are present")
    return(NULL)
  }

  if (pietype == "time") {

    plot_data <- wave_duration(data) %>%
      dplyr::group_by(waveform) %>%
      # duration is in seconds, divide by 60 to get minutes
      dplyr::summarise(waveform = waveform[1], time = round(sum(duration)/60, 2),
                .groups = "drop") %>%
      dplyr::filter(waveform %in% waveforms)

    ## adjust labels to ensure all sum to 100%
    labels <- list()
    labels[[1]] <- round(plot_data$time/(sum(plot_data$time))*100, 1)
    if (sum(labels[[1]]) != 100){
      # find difference (generally positive)
      diff <- 100 - sum(labels)
      # add split diff to all
      labels[[1]] <- labels + diff/length(labels)
      # round, recheck sum, add to first if still not equal
      labels[[1]] <- round(labels[[1]], 1)
      if (sum(labels[[1]]) != 100){
        labels[[1]][1] <- labels[[1]][1] + (100-sum(labels[[1]]))
      }
      labels[[1]] <- paste0(labels[[1]], "%")
    } else {labels[[1]] <- paste0(labels[[1]], "%")}
    labels[[2]] <- paste(plot_data$time, "mins")

    plotly::plot_ly(plot_data, labels = ~waveform, values = ~time, type = 'pie',
                    textinfo = 'text',
                    hoverinfo = 'label+text',
                    hovertext = labels[[2]],
                    text = labels[[1]])
  }

  else if (pietype == "number") {
    plot_data <- wave_occurrence(data) %>%
      dplyr::filter(waveform %in% waveforms)

    ## adjust labels to ensure all sum to 100%
    labels <- round(plot_data$occurrence/(sum(plot_data$occurrence))*100, 1)
    if (sum(labels) != 100){
      # find difference (generally positive)
      diff <- 100 - sum(labels)
      # add split diff to all
      labels <- labels + diff/length(labels)
      # round, recheck sum, add to first if still not equal
      labels <- round(labels, 1)
      if (sum(labels) != 100){
        labels[1] <- labels[1] + (100-sum(labels))
      }
      labels <- paste0(labels, "%")
    } else {labels <- paste0(labels, "%")}

    plotly::plot_ly(plot_data, labels = ~waveform, values = ~occurrence, type = 'pie',
                    textinfo = 'text',
                    hoverinfo = 'label+value',
                    text = labels)
  }


}
