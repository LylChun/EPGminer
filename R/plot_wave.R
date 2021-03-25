#' Plot of EPG data colored by waveform
#'
#' @description The function plot_wave plots EPG data colored by waveform labels.
#'
#' @usage plot_wave(data, facetted = FALSE, aggregate = c("all", "smart", "none"),
#' interval = 100, smart_int = 10)
#'
#' @param data A dataframe containing three columns: time, volts, waveform.
#' @param aggregate Optional aggregation parameter, must be one of "all",
#' "smart", or "none". Defaults to all - data will be aggregated on the chosen
#' interval. If "smart" is chosen, certain regions will be aggregated more strongly
#' than the interval period, e.g. non-probing areas. This is recommended to speed
#' up plotting without losing too much resolution in important waveform areas.
#' Lastly, option none results in no aggregation being performed - all data is plotted.
#' @param smart_int If aggregate = "smart", choose how strongly to further aggregate non-pd
#' sections of the data. Default is 10 for balance between speed and resolution.
#' @inheritParams plot_vts
#' @param facetted TRUE/FALSE indicating whether to plot each waveform
#' individually.
#'
#'
#' @details The function plot_wave relies on specific waveform labels which are
#' as follows: a, C, E1, E2, g, pd1, pd2, pd, and NA OR non-probing for
#' non-feeding activity. The labels must be characters and are case sensitive.
#'
#' @return A plot of EPG data colored by waveform.
#' @export
#'
#' @family waveform functions
#'

plot_wave <- function (data, facetted = FALSE, aggregate = c("all", "smart", "none"),
                       interval = 100, smart_int = 10) {

  time = win = volts = waveform = A = C = E1 = E2 = G =
    pd1 = pd2 = pd = none = nonp = NULL
  rm(list = c("time", "win", "volts", "waveform", "A", "C",
              "E1", "E2", "G", "pd1", "pd2", "pd", "none", "nonp"))

  aggregate = match.arg(aggregate)

  if (aggregate == "none") {
    out = data
  }

  else if (aggregate == "all") {

    out = data %>%
      dplyr::mutate(win = cut(time, breaks = floor(length(time)/interval),
                              labels = FALSE)) %>%
      dplyr::group_by(win) %>%
      dplyr::summarise(time = time[1],
                       volts = mean(volts, na.rm = TRUE),
                       waveform = waveform[1])
  } else {

    out = data %>%
      dplyr::mutate(win = cut(time, breaks = floor(length(time)/interval),
                              labels = FALSE)) %>%
      dplyr::group_by(win) %>%
      dplyr::summarise(time = time[1], volts = mean(volts, na.rm = TRUE),
                       waveform = waveform[1], .groups = "drop") %>%
      dplyr::mutate(win = cut(time, breaks = floor(length(time)/smart_int),
                              labels = FALSE)) %>%
      dplyr::group_by(win) %>%
      dplyr::summarise(time = ifelse(waveform %in% c("pd1", "pd2", "pd"),
                                     time, time[1]),
                       volts = ifelse(waveform %in% c("pd1", "pd2", "pd"),
                                      volts, volts[1]),
                       waveform = ifelse(waveform %in% c("pd1", "pd2", "pd"),
                                         waveform, waveform[1]), .groups = "drop") %>%
      # data is secondly so value of rle(time)[[2]] is equal to row number
      dplyr::slice(rle(time)[[2]])
  }

  if (!facetted) {

    plot_data <- out %>%
      dplyr:: mutate(A = ifelse(waveform == "A", volts, NA),
                     C = ifelse(waveform == "C", volts, NA),
                     E1 = ifelse(waveform == "E1", volts, NA),
                     E2 = ifelse(waveform == "E2", volts, NA),
                     G = ifelse(waveform == "G", volts, NA),
                     pd1 = ifelse(waveform == "pd1", volts, NA),
                     pd2 = ifelse(waveform == "pd2", volts, NA),
                     pd = ifelse(waveform == "pd", volts, NA),
                     none = ifelse(is.na(waveform), volts, NA),
                     nonp = ifelse(waveform == "non-probing", volts, NA))

    ggplot2::ggplot(plot_data) + {
      # dependent on specific names, separate traces because of plotly behavior
      if (!all(is.na(plot_data$A))) ggplot2::geom_line(ggplot2::aes(time, A, color = "A"), na.rm = T)
    } + {
      if (!all(is.na(plot_data$C))) ggplot2::geom_line(ggplot2::aes(time, C, color = "C"), na.rm = T)
    } + {
      if (!all(is.na(plot_data$E1))) ggplot2::geom_line(ggplot2::aes(time, E1, color = "E1"), na.rm = T)
    } + {
      if (!all(is.na(plot_data$E2))) ggplot2::geom_line(ggplot2::aes(time, E2, color = "E2"), na.rm = T)
    } + {
      if (!all(is.na(plot_data$G))) ggplot2::geom_line(ggplot2::aes(time, G, color = "G"), na.rm = T)
    } + {
      if (!all(is.na(plot_data$pd1))) ggplot2::geom_line(ggplot2::aes(time, pd1, color = "pd1"), na.rm = T)
    } + {
      if (!all(is.na(plot_data$pd2))) ggplot2::geom_line(ggplot2::aes(time, pd2, color = "pd2"), na.rm = T)
    } + {
      if (!all(is.na(plot_data$pd))) ggplot2::geom_line(ggplot2::aes(time, pd, color = "pd"), na.rm = T)
    } + {
      if (!all(is.na(plot_data$none))) ggplot2::geom_line(ggplot2::aes(time, none, color = "none"), na.rm = T)
    } + {
      if (!all(is.na(plot_data$nonp))) ggplot2::geom_line(ggplot2::aes(time, nonp, color = "none"), na.rm = T)
    } +
      ggplot2::xlab("Time (in seconds)") + ggplot2::ylab("Volts") +
      ggplot2::scale_colour_manual(name = "Waveform",
                                   values = c("A" = "#1A7A21", "C" = "tan",
                                              "E1" = "blue", "E2" = "black",
                                              "G" = "#F9E15B", "pd1" = "#FDA6F6",
                                              "pd2" = "#C6FA8B", "pd" = "#FF8F20",
                                              "none" = "grey")) +
      ggplot2::theme_minimal()

  } else {
    out <- out %>%
      # index is each waveform/na period
      dplyr::mutate(wave_group = rep(1:length(rle(waveform)[[1]]),
                                     rle(waveform)[[1]])) %>%
      # drop intervening times
      dplyr::filter(!is.na(waveform))

    ggplot2::ggplot(out) +
      ggplot2::geom_line(ggplot2::aes(time, volts, group = 1, color = waveform)) +
      ggplot2::facet_wrap(~wave_group, scales = "free_x") +
      ggplot2::theme_minimal()
  }

}
