################ Utils ################

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


################### A ##############

# run on 24 hr data as a whole
a_label <- function (data, a_o = c(0.75, 0.5, 1, 1.25), a_drop = 0.75) {

  out <- a_times(data, a_o = a_o, a_drop = a_drop)

  return(out)
}

# takes raw data
a_times <- function (data, a_o = c(0.75, 0.5, 1, 1.25), a_drop = 0.75) {

  udat <- a_ao(data, a_o = a_o, a_drop = a_drop)
  times = seq(from = udat$time[1], to = udat$time[2], by = 0.01)

  out = data %>%
    dplyr::mutate(a = dplyr::if_else(round(time, 2) %in% round(times, 2), "A", NA_character_))

  return(out)
}

# takes raw data
a_ao <- function (data, a_o = c(0.75, 0.5, 1, 1.25), a_drop = 0.75) {

  # check for feeding activity at very beginning
  # choose first quantile (lowest value) for beginning, or 0 if feeding immediate
  begin = dplyr::if_else(data$volts[1] > -0.1, quantile(data$volts[1:1000])[[1]], 0)
  # begin = ifelse(data$volts[1] > -0.1, quantile(data$volts[1:1000])[[1]], 0)

  alpha <- data %>%
    # because before feeding, dc is only voltage
    dplyr::filter(volts < begin - a_drop*sd(volts, na.rm = TRUE)) %>%
    dplyr::slice_head() %>%
    dplyr::select(time, volts)

  omega <- data %>%
    dplyr::mutate(sdvolts = sd(volts, na.rm = TRUE)) %>%
    # select time after a start
    dplyr::filter(time >= alpha$time) %>%
    # cut windows to get smoothed state of voltage
    mutate(win = cut(time, breaks = floor(length(time)/25000), labels = FALSE)) %>%
    group_by(win) %>%
    dplyr::summarise(upspike = max(volts) - median(volts), delta = max(volts) - min(volts),
                     time = max(time), volts = utils::tail(volts), sdvolts = sdvolts[1]) %>%
    dplyr::filter(upspike < a_o[1]*sdvolts[1] & upspike > a_o[2]*sdvolts[1] &
                    delta > a_o[3]*sdvolts[1], delta < a_o[4]*sdvolts[1]) %>%
    dplyr::ungroup() %>%
    dplyr::slice_head() %>%
    dplyr::select(time, volts)

  out <- rbind(alpha, omega)

  return(out)
}


########### G ################
# works for one G in 24 hr period
g_label <- function (data, g_drop = 0.75) {

  out <- g_times(data, g_drop)

  return(out)
}


g_times <- function (data, g_drop = 0.75) {

  udat <- g_ao(data, g_drop)

  if (is.null(udat)) { # if no g, ao will return null
    out = data %>%
      dplyr::mutate(g = NA_character_)
  } else {
    times = seq(from = udat$time[1], to = udat$time[2], by = 0.01)

    out = data %>%
      dplyr::mutate(g = dplyr::if_else(round(time, 2) %in% round(times, 2), "G", NA_character_))
    # mutate(g = case_when(
    #   round(time, 2) %in% round(times, 2) ~ "g"
    # ))
  }

  return(out)
}


g_ao <- function (data, g_drop = 0.75) {

  aend <- data %>%
    dplyr::filter(waveform == "a") %>%
    dplyr::slice_tail()

  # check for feeding activity at very beginning
  begin = if_else(data$volts[1] > -0.1, mean(data$volts[1:1000]), 0)
  # begin = ifelse(data$volts[1] > -0.1, mean(data$volts[1:1000]), 0)

  rough <- data %>%
    # cut half hour windows to get smoothed state of voltage
    dplyr::mutate(win = cut(time, breaks = floor(length(time)/180000),
                            labels = FALSE)) %>%
    dplyr::group_by(win) %>%
    # for each half hour period, find average volts
    dplyr::mutate(winavg = mean(volts, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    # filter times before A end
    dplyr::filter(time > aend$time) %>%
    # average volts must be greater than _*sd below beginning
    dplyr::filter(winavg > begin - g_drop*sd(volts, na.rm = TRUE))

  if (is.na(rough$time[1])) { # if no G, return NULL
    out = NULL
  } else {
    rough <- rough %>%
      # because of averaging, will miss beginning and end frames
      dplyr::summarise(time = seq(from = min(time) - 0.5*60*60,
                                  to = max(time) + 0.5*60*60, by = 0.01))


    alpha = data %>%
      # calculate alpha target before filtering down data
      dplyr::mutate(atar = begin - g_drop*sd(volts, na.rm = T)) %>%
      # keep first hour
      dplyr::filter(time >= min(rough$time) & time < (min(rough$time) +
                                                        1*60*60)) %>%
      # filter down to only passing points
      dplyr::filter(volts > atar) %>%
      # calculate time difference to check for outliers
      dplyr::mutate(timediff = c(0, diff(time)))

    if (any(alpha$timediff > 0.5)) {
      alpha <- alpha %>%
        # filter down to time jump points only
        dplyr::filter(timediff > 0.5) %>%
        # select last time jump
        dplyr::slice_tail() %>%
        # leave commented for now, not sure if theoretically sound
        # add time jump from 2 lines above
        # mutate(time = time + timediff) %>%
        select(time, volts)
    } else {
      alpha <- alpha %>%
        dplyr::slice_head() %>%
        dplyr::select(time, volts)
    }


    omega = data %>%
      # calculate omega target before filtering down data
      mutate(otar = begin - g_drop*sd(volts, na.rm = T)) %>%
      # keep last hour
      dplyr::filter(time <= max(rough$time) & time > (max(rough$time) -
                                                        1*60*60)) %>%
      # filter down to only passing points
      dplyr::filter(volts > otar) %>%
      # check if more than 1 sec from previous measurement
      mutate(timediff = c(0, diff(time, 1)),
             # if large gap, then false
             timecheck = dplyr::if_else(timediff < 0.5, TRUE, FALSE)) %>%
      # select first up to first fail (length of first run)
      dplyr::slice(1:rle(timecheck)[[1]][1]) %>%
      # select last time that passes
      tail(1) %>%
      dplyr::select(time, volts)

    out = rbind(alpha, omega)
  }

  return(out)
}


######################## E ################
# run on 24 hr data as a whole
e_label <- function (data, e_var = 0.1, e_o = c(2, 1.25), e_low = 1.25) {

  udat = data %>%
    e_times(., e_var = e_var, e_o = e_o, e_low = e_low) %>%
    select(time, volts, we)
  # find the subforms
  out <- udat %>%
    left_join(e_sub(udat), by = c("time", "volts", "we"))

  ########## replaced, keep for now
  # # bandaid filter
  # # if no deep basin of e then don't bother evaluating
  # if (quantile(data$volts)[1] > -1.5) {
  #   out = data %>%
  #     mutate(subform = NA_character_)
  #   # otherwise, continue
  # } else {
  #   udat = data %>%
  #     # add waveform e label
  #     e_times(., e_var = e_var, e_o = e_o, e_low = e_low) %>%
  #     select(time, volts, we)
  # }
  #
  # if (subform) {
  #   # find the subforms
  #   out <- udat %>%
  #     left_join(e_sub(udat), by = c("time", "volts", "we"))
  #   # if not subform, arbitrary E1
  # } else {
  #   out = udat %>%
  #     mutate(subform = ifelse(we == "e", "E1", NA_character_))
  # }
  ########## End old code
  return(out)
}


# takes raw data
e_ao <- function (data, e_o = c(2, 1.25)) {

  out <- data %>%
    dplyr::mutate(we = dplyr::case_when(
      # will be positive bc voltage drops - bigger minus smaller
      # very large lag because gradual voltage drop
      volts - lead(volts, 75000L) > e_o[1]*sd(volts, na.rm = TRUE) ~
        "start",
      # smaller lag because steeper voltage rise
      # og 5000
      volts - lag(volts, 50000L) > e_o[2]*sd(volts, na.rm = TRUE) ~
        "end"
    )) %>%
    dplyr::filter(!is.na(we)) %>%
    # add grouping index:
    dplyr::mutate(idx = rep(1:length(rle(we)[[1]]), rle(we)[[1]])) %>%
    dplyr::group_by(idx) %>%
    dplyr::mutate(index = dplyr::case_when(
      # og max
      we == "start" ~ which.max(time),
      we == "end" ~ which.min(time)
    )) %>%
    dplyr::slice(index[1])  %>%
    dplyr::ungroup()

  return(out)
}


e_check <- function (data) {

  # if not enough rows, arbitrary false
  if (nrow(data) < 5) {
    out = 10
  } else {
    # receive each "waveform" data
    middle <- data %>%
      # cut each into 5 windows
      mutate(grp = cut(time, breaks = 5)) %>%
      group_by(grp) %>%
      summarise(sdvolts = sd(volts)) %>%
      # select only one section, avoid beginning irregularities
      slice(4)
    out <- middle$sdvolts
  }

  # returns sd in the middle cut
  return(out)
}

elow_check <- function (data) {

  # if not enough rows, arbitrary false
  if (nrow(data) < 3) {
    test = 0
  } else {
    # receive each waveform data
    middle <- data %>%
      # cut each into 3 windows (beginning, bottom, end)
      mutate(grp = cut(time, breaks = 3)) %>%
      group_by(grp) %>%
      summarise(low = mean(volts)) %>%
      # select only middle, should be u bottom
      slice(2)
    test <- middle$low
  }

  return(test)
}

# takes raw data
e_times <- function (data, e_var = 0.1, e_o = c(2, 1.25), e_low = 1.25) {

  ao = e_ao(data, e_o = e_o)

  # if incorrectly picks starting end
  if(slice_head(ao)$we == "end") {
    # drop first
    ao = ao[2:(length(ao$we)), ]
  }

  # if found start with no end, add end bc e is very long
  if(slice_tail(ao)$we == "start") {
    ao = ao %>%
      # time and volts from last row of data
      summarise(time = c(time, max(data$time)), # +25 if later -lag
                volts = c(volts, slice_tail(data)$volts),
                we = c(we, "end"))
  }

  # remove start/end that ids an e with incorrect duration
  ao <- ao %>%
    mutate(time = case_when(
      # duration less than 25 secs (impossible with lag)
      # filter to longer than 3 minutes (for now)
      we == "start" & lead(time) - time < 3*60 ~ NA_real_,
      we == "end" & time - lag(time)  < 3*60 ~ NA_real_,
      TRUE ~ time
    )) %>%
    filter(!is.na(time))

  times <- unlist(seqvec(from = ao$time[ao$we == "start"],
                         # could subtract 25 to "remove" the effects of lag (see e_ao)
                         to = ao$time[ao$we == "end"], by = 0.01))

  out <- data %>%
    mutate(we = if_else(round(time, 2) %in% round(times, 2), "e", "non-e"),
           # case_when(round(time, 2) %in% round(times, 2) ~ "e", TRUE ~ "non-e"),
           sdvolts = sd(volts),
           low_tar = mean(volts, na.rm = TRUE) - e_low*sdvolts[1]
    ) %>%
    # group by each individual waveform
    mutate(idx = rep(1:length(rle(we)[[1]]), rle(we)[[1]])) %>%
    group_by(idx) %>%
    # set targets
    mutate(e_tar = e_check(data.frame(time, volts)),
           low_test = elow_check(data.frame(time, volts)),
           low_pf = low_test < low_tar,
           # only pass if low sd
           e_pf = e_tar < e_var*sdvolts[1],
           we = case_when(
             low_pf == FALSE ~ "non-e",
             e_pf == FALSE ~ "non-e",
             TRUE ~ we )) %>%
    ungroup()

  return(out)
}


# receives full e labeled data
e_mid <- function (data) {

  if (!any(data$we == "e")) { # if no e, midpoint is NA
    out = data %>% mutate(subform = NA)
  } else {
    out <- data %>%
      # add grouping index:
      mutate(idx = rep(1:length(rle(we)[[1]]), rle(we)[[1]])) %>%
      # select only each e instance
      filter(we == "e") %>%
      group_by(idx) %>%
      # cut _ second windows to capture spikes
      mutate(win = cut(time, breaks = floor(length(time)/500),
                       labels = FALSE)) %>%
      group_by(win) %>%
      # bc E2 shows higher amplitude compared to E1
      # those large negative voltage spikes
      mutate(test = length(volts[volts > mean(volts)])/length(volts)) %>%
      # set target for percent above quantile
      mutate(target = quantile(test)[4],
             subform = if_else(test > target, "E2", "E1")) %>%
             #   case_when(
             #   # E2 has larger percent above because only spikes below
             #   test > target ~ "E2",
             #   # anything else labeled E1
             #   TRUE ~ "E1"
             # )) %>%
      ungroup()
  }

  if (!is.na(out$subform[1])) { # only if e actually exists check for only E1
    # if quantile is only sampling on E1 waveform, the mean test will be low
    if (mean(out$test, na.rm = TRUE) <= 0.5) {
      # since no E2, return no midpoint
      out = out %>% mutate(subform = "none")
    } else if (!any(out$subform == "E2")) { # if no identified E2
      out = out %>% mutate(subform = "none")
    } else { # has E, has E2
      out <- out %>%
        group_by(idx) %>%
        filter(subform == "E2") %>%
        slice(which.min(time)) %>%
        ungroup()
    }
  }

  # returns time, volts, of midpoint
  return(out)
}


# receive full e labeled
e_sub <- function (data) {

  mid = e_mid(data)

  if (is.na(mid$subform[1])) { # no E, midpoint is NA, subforms NA as well
    out <- data %>%
      mutate(subform = NA_character_)
  } else if (mid$subform[1] == "none") { # only E1, midpoint is 'none'
    out <- data %>%
      # select only waveform e times
      filter(we == "e") %>%
      mutate(subform = "E1")
  } else { # has e and has midpoint
    out <- data %>%
      # add grouping index:
      mutate(idx = rep(1:length(rle(we)[[1]]), rle(we)[[1]])) %>%
      # select only each e instance, group
      filter(we == "e") %>%
      group_by(idx) %>%
      mutate(midpoint = if_else(round(time, 2) %in% round(mid$time, 2), "mid", "filler"),
        #        case_when(
        # round(time, 2) %in% round(mid$time, 2) ~ "mid",
        # TRUE ~ "filler"), # put in mid times
        subform = case_when(
          # could adjust times here if warranted
          time <= time[midpoint == "mid"] ~ "E1",
          time > time[midpoint == "mid"] ~ "E2"
        ))  %>% # since already grouped, time ranges per e
      ungroup()
  }

  return(out)
}

################# pd ##################
pd_label <- function (data, ...) {

  # if no pd exists in the data, return NA for pdsubform/pd_group
  if (is.null(pd_times(data, ...))) {
    out = data %>%
      mutate(pdsubform = NA_character_)
    # otherwise, proceed
  } else {
    # find the whole pd before separation into subforms
    whole <- pd_times(data, ...) %>%
      select(time, volts, pd)

    # find the pd subforms within each pd
    out <- whole %>%
      left_join(pd_sub(whole), by = c("time", "volts", "pd"))
  }

  return(out)
}

# receive data without a, e, and g
pd_ao <- function (data, pd_win = 1) {

  out <- data %>%
    mutate(win = ggplot2::cut_number(rows, n = pd_win, labels = FALSE)) %>%
    group_by(win) %>%
    mutate(relsd = sd(volts)) %>%
    # return to original ungrouped format, now with relative sd
    ungroup() %>%
    mutate(drop = volts - lead(volts, 1000L),
           rise = volts - dplyr::lag(volts, 700L),
           pd = case_when(
             drop  > 2.75*relsd ~ "start",
             # rise must start from trough
             rise > 2*relsd & lag(volts, 700) <
               (mean(volts) -1.5*sd(volts)) ~ "end"
           )) %>%
    dplyr::filter(!is.na(pd)) %>%
    # add grouping index to group starts and ends
    mutate(idx = rep(1:length(rle(pd)[[1]]), rle(pd)[[1]])) %>%
    group_by(idx) %>%
    mutate(index = dplyr::if_else(pd[1] == "start", which.max(drop), which.max(rise))
    ) %>%
    # select best fit starts and ends
    slice(index[1]) %>%
    ungroup() %>%
    select(time, pd)

  return(out)
}

# helper to define u shape
u_check <- function (data) {

  # receive each "pd" data
  middle <- data %>%
    # cut each "pd" into 3 windows (beginning, bottom, end)
    mutate(grp = cut(time, breaks = 3)) %>%
    group_by(grp) %>%
    summarise(middle = min(volts)) %>%
    # select only middle, should be u bottom
    slice(2)

  # test should be larger for real pd
  test = mean(data$volts[1], data$volts[length(data$volts)], na.rm = TRUE) -
    middle$middle

  return(test)
}

# helper to check for voltage drop
low_check <- function (data) {

  # if not pd, could have non-consec times; arbitrary false
  if (data$pd[1] == "non-pd") {
    out = 0
  } else {
    # receive each "pd" data
    middle <- data %>%
      # cut each "pd" into 3 windows (beginning, bottom, end)
      mutate(grp = cut(time, breaks = 3)) %>%
      group_by(grp) %>%
      summarise(low = mean(volts)) %>%
      # select only middle, should be u bottom
      slice(3)
      # slice(low_sl)
    out <- middle$low
  }

  # returns mean in the middle cut
  return(out)
}

# default one window, otherwise indicate # desired
# e.g. if half hour windows, give 2x the data length (hours)
# ... allows to select low_check: low_sl value
pd_times <- function (data, pd_low = 1.25, pd_win = 1) {

  # get starts and ends
  udat <- pd_ao(data, pd_win = pd_win) %>%
    ungroup()

  # if incorrectly picks up ending start
  if(slice_tail(udat)$pd == "start") {
    # drop last by indexing to penultimate
    udat = udat[1:(length(udat$pd) - 1), ]
  }

  # if incorrectly picks starting end
  if(slice_head(udat)$pd == "end") {
    # drop first
    udat = udat[2:(length(udat$pd)), ]
  }

  # remove start/end that ids a pd with incorrect duration
  udat <- udat %>%
    mutate(time = case_when(
      # duration less than 20secs
      pd == "start" & lead(time) - time <= 20 ~ NA_real_,
      pd == "end" & time - lag(time)  <= 20 ~ NA_real_,
      # duration greater than 1.5 minutes (avg duration around 45 secs)
      pd == "start" & lead(time) - time >= 5*60 ~ NA_real_,
      pd == "end" & time - lag(time)  >= 5*60 ~ NA_real_,
      TRUE ~ time
    )) %>%
    filter(!is.na(time))

  times <- unlist(seqvec(from = udat$time[udat$pd == "start"],
                         to = udat$time[udat$pd == "end"],
                         by = 0.01))

  out <- data %>%
    mutate(pd = if_else(round(time, 2) %in% round(times, 2), "pd", "non-pd")) %>%
    #          case_when(
    #   # times between id'ed starts and ends labeled pd
    #   round(time, 2) %in% round(times, 2) ~ "pd",
    #   TRUE ~ "non-pd"
    # )) %>%
    # below is to remove non u shaped:
    #
    # group by each individual pd/non-pd
    mutate(idx = rep(1:length(rle(pd)[[1]]), rle(pd)[[1]])) %>%
    group_by(idx) %>%
    # currently test returns test value, can do tf later
    # to eliminate redudancy
    mutate(test = u_check(data.frame(time, volts))) %>%
    ungroup() %>%
    # need get relative sd again to determine
    # cut windows to get relative sd
    mutate(win = cut_number(rows, n = pd_win, labels = FALSE)) %>%
    # mutate(win = cut(rows, breaks = ceiling(length(rows)/180000),
    # labels = FALSE)) %>%
    # mutate(win = 1) %>%
    group_by(win) %>%
    mutate(relsd = sd(volts),
           lowtar = mean(volts, na.rm = T) - pd_low*relsd) %>%
    ungroup() %>%
    # group by each individual pd/non-pd, for relsd breakpoint
    # in middle of pd
    mutate(idx = rep(1:length(rle(pd)[[1]]), rle(pd)[[1]])) %>%
    group_by(idx) %>%
    mutate(u_thresh = 3*mean(relsd, na.rm = T),
           low_thresh = low_check(data.frame(time, volts, pd)),
           u_pf = test > u_thresh,
           # min needs to be lower than low target
           low_pf = low_thresh < lowtar,
           pd = case_when(
             # if u_pf fails, non-pd
             u_pf == FALSE ~ "non-pd",
             # if low_pf fails, non-pd
             low_pf == FALSE ~ "non-pd",
             TRUE ~ pd )
    ) %>%
    ungroup()

  # return data frame with pd label
  return(out)
}

# will receive zebras output
pd_mid <- function (data) {

  out <- data %>%
    # cut _ second windows to get relative sd
    mutate(win = cut(time, breaks = ceiling(length(time)/180000),
                     labels = FALSE)) %>%
    group_by(win) %>%
    # find sd for each _ sec window
    mutate(relsd = sd(volts)) %>%
    # ungroup to return to original
    ungroup() %>%
    # index is each pd or non pd period
    mutate(pd_group = rep(1:length(rle(pd)[[1]]), rle(pd)[[1]])) %>%
    filter(pd == "pd") %>%
    # group to each individual pd
    group_by(pd_group) %>%
    # rough pick of drops that could be the correct one
    mutate(drop = lag(volts, 300L) - volts,
           midpoint = case_when(
             # want sig drop that's not from large beginnning
             drop > 0.25*relsd &
               drop < 2*relsd ~ "mid"
           )) %>%
    # filter out points b4 halfway point (pd1 longer)
    # and filter after 75% way through
    filter(time >= quantile(time)[3] &
             time <= quantile(time)[4]) %>%
    # filter down to possible midpoints
    filter(!is.na(midpoint)) %>%
    # choose largest drop
    slice(which.max(drop)) %>%
    # to drop pd_group
    ungroup() %>%
    select(time, volts, midpoint)
  return(out)
}

# must receive "whole" data
pd_sub <- function (data) {

  out <- data %>%
    # whales is output of pd_mid
    left_join(pd_mid(data), by = c("time", "volts")) %>%
    # group each pd or non pd period
    mutate(pd_group = rep(1:length(rle(pd)[[1]]), rle(pd)[[1]])) %>%
    filter(pd == "pd") %>%
    group_by(pd_group) %>%
    # filter out "pds" without midpoint
    filter(any(midpoint == "mid")) %>%
    mutate(midpoint = dplyr::if_else(is.na(midpoint), "filler", midpoint)) %>%
    #          case_when(
    #   is.na(midpoint) ~ "filler",
    #   TRUE ~ midpoint
    # )) %>%
    mutate(pdsubform = case_when(
      # when midpoint does not exist, list as NA, not pd
      # probably unnecesary see above
      # is.na(midpoint == "mid") ~ NA_character_,
      time <= time[midpoint == "mid"] ~ "pd1",
      # may need to re-add one second lag
      time > time[midpoint == "mid"] ~ "pd2"
    ))

  return(out)
}

##################### Wave ##################
# shiny specific function
wave_label_eg <- function (data, e_var = 0.1, e_o = c(2, 1.25), e_low = 1.25,
                           g_drop = 0.75) {

  edat <- data %>%
    # a use smoothed data to label, but use raw data for actual kept volts
    mutate(e = e_label(data, e_var = e_var, e_o = e_o, e_low = e_low)$subform,
           waveform = case_when(
             !is.na(waveform) ~ waveform,
             !is.na(e) ~ e,
             TRUE ~ NA_character_))

  out <- edat %>%
    mutate(g = g_label(., g_drop)$g,
           waveform = case_when(
             !is.na(waveform) ~ waveform,
             !is.na(g) ~ g,
             TRUE ~ NA_character_))

  return(out)
}
# potentially same as stand-alone, unsure
wave_label_pdc <- function (data, ...) {

  addg <- data

  # find end for removing pdformat below
  aend <- addg %>%
    filter(waveform == "a") %>%
    slice_tail()

  udat <- addg %>%
    # filter out e, g, and a
    filter(is.na(waveform)) %>%
    # filter early times before feeding, a, and 60s after a
    filter(time >= aend$time + 1*60) %>%
    # add rows column to cut windows on
    mutate(rows = 1:n()) %>%
    select(-waveform)

  # find pd times
  pds <- pd_label(udat, ...) %>%
    filter(pd == "pd") %>%
    mutate(time = round(time, 2))

  if (any(!is.na(addg$g))) {
    gend <- addg %>%
      filter(waveform == "G") %>%
      slice_tail()

    gtimes <- seq(gend$time + 0.01, gend$time + 500, by = 0.01)
  } else {gtimes = NA}

  # add in pds
  withpd <- addg %>%
    # wopd has all times, pds only has pd times
    left_join(pds, by = c("time", "volts")) %>%
    mutate(waveform = case_when(
      !is.na(waveform) ~ waveform,
      !is.na(pdsubform) ~ pdsubform,
      TRUE ~ NA_character_
    )) %>%
    mutate(waveform = ifelse(
      # if close to G, wipe out
      round(time, 2) %in% round(gtimes, 2), NA, waveform
    )) %>%
    select(time, volts, waveform)

  # add waveform C, fix A
  out <- withpd %>%
    mutate(waveform = case_when(
      # first set no activity + A as pre-labeled
      time <= aend$time ~ waveform,
      # any remaining intervening time becomes C
      is.na(waveform) ~ "C",
      # keep all other labels
      TRUE ~ waveform
    ))

  return(out)
}

################# Probe ###################

# works for n48, s236, s196
probe_a <- function (data) {

  # check for feeding activity at very beginning
  begin = ifelse(data$volts[1] > -0.1, mean(data$volts[1:1000]), 0)

  data <- epgminer:::smooth_vts(data, 127)

  bandpass <- data %>%
    # cut windows to get smoothed state of voltage
    mutate(win = cut(time, breaks = floor(length(time)/5000),
                     labels = FALSE)) %>%
    group_by(win) %>%
    # for each period, find average volts
    mutate(winpeak = max(volts, na.rm = TRUE),
           winval = min(volts, na.rm = TRUE)) %>%
    ungroup() %>%
    # mutate(high = winpeak > begin - probe_drop*sd(volts, na.rm = T)) %>%
    # average volts must be greater than _*sd below beginning
    # og is 0.75sd
    mutate(hightar = (max(volts, na.rm = T) + min(volts, na.rm = T))/3,
           highpass = winpeak > (max(volts, na.rm = T) + min(volts, na.rm = T))/3,
           lowtar = (max(volts, na.rm = T) + min(volts, na.rm = T))/2,
           lowpass = winval < lowtar)

  possible <- bandpass %>%
    mutate(bandpass = ifelse(highpass & lowpass, TRUE, FALSE)) %>%
    # i think need to rle from high/low passes together
    # start here
    mutate(idx = rep(1:length(rle(bandpass)[[1]]), rle(bandpass)[[1]])) %>%
    group_by(idx) %>%
    mutate(exit = ifelse(highpass == TRUE & lowpass == TRUE &
                           # seems the length filter isn't working properly?
                           length(idx) > 20000, TRUE, FALSE),
           tmp = length(idx))

  exit <- possible[possible$exit, ]

  starts <- exit %>%
    select(time, volts, idx) %>%
    group_by(idx) %>%
    slice(c(1, n())) %>%
    mutate(ao = c("start", "end")) %>%
    ungroup() %>%
    mutate(bridge = case_when(
      ao == "start" & (time - lag(time) < 100) ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    filter(!bridge & ao == "start")

  return(starts)
}

# works
probe_o <- function (data) {

  starts <- probe_a(data)

  data <- epgminer:::smooth_vts(data, 127)

  ends <- list()
  for (i in 1:nrow(starts)) {
    search <- data %>%
      filter(time >= starts$time[i]) %>%
      mutate(hightar = (max(volts, na.rm = T) + min(volts, na.rm = T))/4,
             highpass = volts > hightar)

    gaps <- search %>%
      mutate(idx = rep(1:length(rle(highpass)[[1]]), rle(highpass)[[1]])) %>%
      group_by(idx) %>%
      mutate(size = length(idx)) %>%
      filter(size > 10000) %>%
      ungroup() %>%
      slice(which(highpass)[1]:n())

    ends[[i]] <- gaps[which(!gaps$highpass), ] %>%
      slice(1)
  }

  out <- ends %>%
    Reduce(function(dtf1,dtf2) rbind(dtf1, dtf2), .)

  return(out)
}

# if there is non-probing activity in the middle
# returns each section as list entry
probe_split <- function (data) {
  a <- c(probe_a(data)$time, max(data$time))
  o <- probe_o(data)$time

  splits <- list()
  splits[[1]] <- data %>%
    filter(time <= a[1])
  for (i in 1:length(o)) {
    splits[[i + 1]] <- data %>%
      filter(time >= o[i] & time < a[i+1])
  }

  # remove empty list entries
  idx <- c()
  for (i in 1:length(splits)) {
    idx[i] <- dplyr::if_else(dim(splits[[i]])[1] == 0, FALSE, TRUE)
  }

  return(splits[idx])
}

# probe_split_old <- function (data) {
#   a <- c(probe_a(data)$time, max(data$time))
#   o <- probe_o(data)$time
#
#   splits <- list()
#   splits[[1]] <- data %>%
#     filter(time <= a[1])
#   for (i in 1:length(o)) {
#     splits[[i + 1]] <- data %>%
#       filter(time >= o[i] & time < a[i+1])
#   }
#
#   return(splits)
# }


# new dev, untested
probe_apply <- function (data) {
  # take in probe_split data
  split <- probe_split(data)

  as <- list()
  for (i in 1:length(split)) {
    as[[i]] <- a_ao(split[[i]], a_o = c(0.75, 0.5, 1, 1.25), a_drop = 0.75)
  }

  out <- rbindlist(as)
  return(out)
}

# want to take data, remove probe and a sections
# receive a labelled data - a_data_probe, shiny only
probe_comb <- function (data, e_var = 0.1, g_drop = 0.75) {

  feed <- rbindlist(probe_split(data))

  # using a_labeled data (has all time points)
  adat <- data %>%
    # keep only splits (feeding times)
    filter(round(time, 2) %in% round(feed$time, 2))

  # will need different wave_label functions

  out <- wave_label_probe(adat, e_var = e_var, g_drop = g_drop)

  return(out)
}

# take in a_labeled data
wave_label_probe <- function (data, e_var = 0.1, e_o = c(2, 1.25), e_low = 1.25,
                              g_drop = 0.75, ...) {

  edat <- data %>%
    mutate(e = e_label(data, e_var = e_var, e_o = e_o, e_low = e_low)$subform,
           waveform = case_when(
             !is.na(waveform) ~ waveform,
             !is.na(e) ~ e,
             TRUE ~ NA_character_))

  aeg <- edat %>%
    # technically will only find g is after last bit of a
    # right now think no g if nonprobe middle, to be fixed later perhaps
    mutate(g = g_label(., g_drop)$g,
           waveform = case_when(
             !is.na(waveform) ~ waveform,
             !is.na(g) ~ g,
             TRUE ~ NA_character_))

  # find feeding beginning
  a_a <- aeg %>%
    filter(waveform == "a") %>%
    slice_head()

  pdform <- aeg %>%
    # filter out aeg and non-feed before first a
    filter(is.na(waveform) & time >= a_a$time) %>%
    filter(time >= a_a$time) %>%
    # add rows column to cut windows on
    mutate(rows = 1:n()) %>%
    select(time, volts, rows)

  # find pd times
  pds <- pd_label(pdform, ...) %>%
    filter(pd == "pd") %>%
    mutate(time = round(time, 2))

  if (any(!is.na(aeg$g))) {
    gend <- aeg %>%
      filter(waveform == "g") %>%
      slice_tail()

    gtimes <- seq(gend$time + 0.01, gend$time + 500, by = 0.01)
  } else {gtimes = NA}

  # add in pds
  withpd <- aeg %>%
    left_join(pds, by = c("time", "volts")) %>%
    mutate(waveform = case_when(
      !is.na(waveform) ~ waveform,
      !is.na(pdsubform) ~ pdsubform,
      TRUE ~ NA_character_
    )) %>%
    mutate(waveform = ifelse(
      # if close to G, wipe out
      round(time, 2) %in% round(gtimes, 2), NA, waveform
    )) %>%
    select(time, volts, waveform)

  # add waveform C, fix A
  out <- withpd %>%
    mutate(waveform = case_when(
      # first set no activity as pre-labeled
      time <= a_a$time ~ waveform,
      # any remaining intervening time becomes C
      is.na(waveform) ~ "C",
      # keep all other labels
      TRUE ~ waveform
    ))
}
