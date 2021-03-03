library(data.table)
library(dplyr)
library(epgminer)
library(plotly)
library(readr)
library(shiny)
library(shinythemes)
library(stringr)

shinyServer(function(input, output) {

  ################ Label My Data #################

  la_data <- reactive ({

    validate (
      need (input$data, "Waiting on Data Upload...")
    )

    read_epg(input$data$datapath, ext = "csv")
  })

  ana_data <- reactive ({

    validate (
      need (input$anadata, "Waiting on Data Upload...")
    )

    read_epg(input$anadata$datapath, ext = "ANA")
  })

  raw_data <- reactive ({

    if (input$rawtype == "csv") {
      req(input$rawdata)
      out <- read_csv(input$rawdata$datapath)
    }

    else if (input$rawtype == "txt") {
      req(input$rawdata2)

      list <- lapply(input$rawdata2$datapath, read_epg)
      out <- rbindlist(list)
    }

    return(out)
  })

  comp_raw <- reactive ({

    validate (
      need (input$compraw, "Waiting on Data Upload...")
    )

    list <- lapply(input$compraw$datapath, read_epg)
    out <- rbindlist(list)
    return(out)
  })

  analyze_data <- reactive ({

    data_id <- showNotification("Rendering...", duration  = NULL, closeButton = FALSE)
    on.exit(removeNotification(data_id), add = TRUE)

    if (input$label == "prelab") {
      data = la_data()
    }

    else if (input$label == "ana") {
      data = label_ana(raw_data(), ana_data())
    }

    # technically useless as data table not printed for comp
    else if (input$label == "comp") {
      data = comp_label()
    }

    return(data)
  })

  output$data <- DT::renderDataTable({

    DT::datatable(analyze_data()[1:5, ],
                  options = list(dom = "t"),
                  rownames = FALSE)
  })

  ################### Analyze My Data ####################

  metric_type <- reactive ({

    if (input$metric == "freq") {
      out <- wave_topfreq(analyze_data())
      colnames(out) <- c("group", "waveform", "frequency")
      out <- out[, 2:3]
    }

    else if (input$metric %in% c("dur", "count")) {
      out <- wave_duration(analyze_data())
      colnames(out) <- c("group", "waveform", "duration")
      out <- out
    }

    return(out)
  })

  metric_tab <- reactive ({

    out <- metric_type()

    if (input$metric == "freq") {

      if (input$summary == "default") {
        out <- out
      }

      else if (input$summary == "median") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    topfreq = median(frequency))
      }

      else if (input$summary == "mean") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    topfreq = mean(frequency))
      }

      else if (input$summary == "sd") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    sd = sd(frequency))
      }

    }

    else if (input$metric == "dur") {

      if (input$summaryd == "default") {
        out <- out[, 2:3]
      }

      else if (input$summaryd == "mean") {
        out <- out[, 2:3] %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    duration = mean(duration))
      }

      else if (input$summaryd == "median") {
        out <- out[, 2:3] %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    duration = median(duration))
      }

      else if (input$summary == "sd") {
        out <- out[, 2:3] %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    sd = sd(duration))
      }

    }

    else if (input$metric == "count") {
      out <- out %>%
        group_by(waveform) %>%
        summarise(waveform = waveform[1],
                  count = length(unique(group)))
    }

    return(out)
  })

  output$metric <- DT::renderDataTable({

    metric_id <- showNotification("Rendering...", duration  = NULL, closeButton = FALSE)
    on.exit(removeNotification(metric_id), add = TRUE)

    DT::datatable(metric_tab(), extensions = "Buttons",
                  options = list(dom = "Btip", paging = FALSE,
                                 buttons = c("copy", "csv", "excel", "pdf", "print")))
  })

  ###################### Visuals #####################

  visual_data <- reactive ({

    if (input$plottype == "fbar") {
      out <- wave_topfreq(analyze_data())
      colnames(out) <- c("group", "waveform", "frequency")
      out <- out[, 2:3]
    }

    else if (input$plottype == "pie") {
      out <- analyze_data()
    }

    return(out)
  })

  plot_react <- reactive({

    plot_id <- showNotification("Rendering...", duration  = NULL, closeButton = FALSE)
    on.exit(removeNotification(plot_id), add = TRUE)

    if (input$plottype == "fbar") {
      ggplot(visual_data()) + geom_boxplot(aes(waveform, frequency))
    }

    else if (input$plottype == "pie") {

      if (input$pietype == "pie_t") {
        plot_data <- visual_data() %>%
          mutate(wave_group = rep(1:length(rle(waveform)[[1]]),
                                  rle(waveform)[[1]])) %>%
          filter(!is.na(waveform)) %>%
          mutate(wave_group = rep(1:length(rle(wave_group)[[1]]),
                                  rle(wave_group)[[1]])) %>%
          group_by(wave_group) %>%
          summarise(waveform = waveform[1], length = length(time))

        ggplot(data = plot_data, mapping = aes(x = 1, fill = waveform, y = length)) +
          geom_bar(stat = "identity") + coord_polar("y", start = 0) + theme_void()
      }

      else if (input$pietype == "pie_c") {
        plot_data <- visual_data() %>%
          mutate(wave_group = rep(1:length(rle(waveform)[[1]]),
                                  rle(waveform)[[1]])) %>%
          filter(!is.na(waveform)) %>%
          mutate(wave_group = rep(1:length(rle(wave_group)[[1]]),
                                  rle(wave_group)[[1]])) %>%
          group_by(wave_group) %>%
          summarise(wave_group = wave_group[1], waveform = waveform[1])

        ggplot(data = plot_data, mapping = aes(x = 1, fill = waveform)) +
          geom_bar() + coord_polar("y", start = 0) + theme_void()
      }
    }

  })

  output$plot <- renderPlot({
    plot_react()
  })

  visual_name <- reactive({

    if (input$plottype == "fbar") {
      out <- "Frequency_barplot"
    }

    else if (input$plottype == "pie") {
      out <- "Waveform_piechart"
    }

    return(out)
  })

  options(shiny.usecairo = TRUE)

  output$pdf <- downloadHandler(
    filename = function() {
      paste(visual_name(), '.pdf', sep = '')
    },
    content = function(file) {
      cairo_pdf(filename = file, width = 10, height = 7, bg = "transparent")
      plot(plot_react())
      dev.off()
    }
  )

  output$png <- downloadHandler(
    filename = function() {
      paste(visual_name(), '.png', sep = '')
    },
    content = function(file) {
      png(file)
      plot(plot_react())
      dev.off()
    }
  )

  output$eps <- downloadHandler(
    filename = function() {
      paste(visual_name(), '.eps', sep = '')
    },
    content = function(file) {
      postscript(file)
      plot(plot_react())
      dev.off()
    }
  )

  ######################## Algorithm ########################


  comp_raw <- reactive ({
    req(input$compraw)

    list <- lapply(input$compraw$datapath, read_epg)
    out <- rbindlist(list)
    return(out)
  })

  a_data <- reactive ({

    # shiny specific function
    wave_label_a <- function (data, ao) {

      out <- data %>%
        mutate(waveform = ifelse(time >= ao[1] & time <= ao[2], "a", NA_character_))

      return(out)
    }

    ao <- as.double(str_split(input$in_ao, pattern = ",")[[1]])
    out <- wave_label_a(comp_raw(), ao)

    return(out)
  })

  auto_data <- reactive ({

    req(input$in_evar)

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

    eg <- wave_label_eg(a_data(), e_var = as.double(input$in_evar))
    out <- wave_label_pdc(eg)

    return(out)
  })

  plot_comp <- reactive ({


    validate (
      need (!is.null(input$compraw), "Waiting on Data Upload..."),
      need (!is.null(input$in_ao), message = FALSE)
    )

    if (input$adone == "n") {
      plot_wave(a_data(), aggregate = "smart")
    }

    else if (input$adone == "y" & input$pd_manual == "n") {

      validate (
        need (!is.null(input$in_evar), "Please Wait - Rendering")
      )

      plot_wave(auto_data(), aggregate = "smart")
    }

    else if (input$pd_manual == "y") {
      req(!is.null(points))

      plot_wave(comp_label())
    }

  })

  comp_label <- reactive ({

    if (input$pd_manual == "n") {
      out <- auto_data()
    }

    else if (input$pd_manual == "y"){

      semi_data <- auto_data()

      a_idx <- seq(1, by = 2, length.out = nrow(points_react())/2)
      o_idx <- seq(2, by = 2, length.out = nrow(points_react())/2)
      pdtimes <- unlist(seqvec(from = points_react()$time[a_idx],
                               to = points_react()$time[o_idx],
                               by = 0.01))

      out <- semi_data %>%
        mutate(waveform = ifelse(round(time, 2) %in% round(pdtimes, 2),
                                 "pd", waveform))
    }

    return(out)
  })

  output$comp_plot <- renderPlotly({

    comp_id <- showNotification("Rendering...", duration  = NULL, closeButton = FALSE)
    on.exit(removeNotification(comp_id), add = TRUE)

    # has been modified bc plotly can't have multicolored single trace
    # plot_wave <- function (data, facetted = FALSE, aggregate = TRUE,
    #                        pd = TRUE, e = TRUE, g = TRUE, pre_label = T) {
    #   if (!pre_label) {
    #     udat <- wave_label(data, pd = pd, e = e, g = g)
    #   } else {udat = data}
    #
    #   if (aggregate) {
    #     out = udat %>%
    #       mutate(win = cut(time, breaks = floor(length(time)/100),
    #                        labels = FALSE)) %>%
    #       group_by(win) %>%
    #       summarise(time = time[1],
    #                 volts = mean(volts, na.rm = TRUE),
    #                 waveform = waveform[1])
    #   } else {out = udat}
    #
    #   if (!facetted) {
    #
    #     plot_data <- out %>%
    #       mutate(A = ifelse(waveform == "a", volts, NA),
    #              C = ifelse(waveform == "C", volts, NA),
    #              E1 = ifelse(waveform == "E1", volts, NA),
    #              E2 = ifelse(waveform == "E2", volts, NA),
    #              G = ifelse(waveform == "g", volts, NA),
    #              pd1 = ifelse(waveform == "pd1", volts, NA),
    #              pd2 = ifelse(waveform == "pd2", volts, NA),
    #              pd = ifelse(waveform == "pd", volts, NA),
    #              none = ifelse(is.na(waveform), volts, NA))
    #
    #     ggplot(plot_data) + {
    #       # dependent on specific names, might need to add "pd"
    #       if (!all(is.na(plot_data$A))) geom_line(aes(time, A), color = "#1A7A21")
    #     } + {
    #       if (!all(is.na(plot_data$C))) geom_line(aes(time, C), color = "tan")
    #     } + {
    #       if (!all(is.na(plot_data$E1))) geom_line(aes(time, E1), color = "blue")
    #     } + {
    #       if (!all(is.na(plot_data$E2))) geom_line(aes(time, E2), color = "black")
    #     } + {
    #       if (!all(is.na(plot_data$G))) geom_line(aes(time, G), color = "#F9E15B")
    #     } + {
    #       if (!all(is.na(plot_data$pd1))) geom_line(aes(time, pd1), color = "#FDA6F6")
    #     } + {
    #       if (!all(is.na(plot_data$pd2))) geom_line(aes(time, pd2), color = "#C6FA8B")
    #     } + {
    #       if (!all(is.na(plot_data$pd))) geom_line(aes(time, pd), color = "#F7AC74")
    #     } + {
    #       if (!all(is.na(plot_data$none))) geom_line(aes(time, none), color = "grey")
    #     }
    #
    #   } else {
    #     out <- out %>%
    #       # index is each waveform/na period
    #       mutate(wave_group = rep(1:length(rle(waveform)[[1]]),
    #                               rle(waveform)[[1]])) %>%
    #       # drop intervening times
    #       filter(!is.na(waveform))
    #
    #     ggplot(out) +
    #       geom_line(aes(time, volts, group = 1, color = waveform)) +
    #       facet_wrap(~wave_group, scales = "free_x")
    #   }
    #
    # }

    ggplotly(plot_comp())

  })

  output$ao <- renderUI ({

    ao <- a_ao(comp_raw(), a_o = c(0.75, 0.5, 1, 1.25), a_drop = 0.75)$time

    textInput("in_ao", "Specify A start and end times",
              value = paste(ao[1], ", ", ao[2], sep = ""))

  })

  output$e_var <- renderUI ({
    textInput("in_evar", "Specify acceptable E Variance", value = 0.2)
  })

  values <- reactiveValues()

  observe ({
    req(input$in_ao)

    # test stop removal of points after add manual
    validate(
      need(input$pd_manual == "n", message = F)
    )

    event <- event_data("plotly_click")
    values$pd1 <- event
  })

  points <- data.frame()
  points_react <- reactive ({

    if (!is.null(values$pd1)) {
      points <<- rbind(points, values$pd1[3:4])
      colnames(points) <- c("time", "volts")
      points <- points %>%
        arrange(time)
    }

    if (is.null(values$pd1)) {
      points <<- data.frame()
    }

    return(points)
  })

  output$comp_table <- renderTable ({

    return(points_react())
  })

  observeEvent(input$clear, {
    values$pd1 = points = NULL
    rm(list = c("points"))
  })

  output$downloadcomp <- downloadHandler(
    filename = function() {
      filename <- str_sub(input$compraw$name, end = -8)
      paste(filename, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(comp_label(), file, row.names = FALSE)
    }
  )

  ################## Beta Zone ####################

  # beta_data <- reactive ({
  #   req(input$beta)
  #
  #   list <- lapply(input$beta$datapath, read_epg)
  #   out <- rbindlist(list)
  #   return(out)
  # })
  #
  # output$data_probe <- DT::renderDataTable({
  #
  #   DT::datatable(beta_data()[1:5, ],
  #                 options = list(dom = "t"),
  #                 rownames = FALSE)
  # })
  #
  # output$probe_a <- renderUI ({
  #
  #   alphas <- probe_apply(beta_data())$time
  #
  #   idx <- seq(1, by = 2, length.out = length(alphas)/2)
  #
  #   textInput("in_a", "Specify A start(s)",
  #             value = paste(alphas[idx], collapse = ","))
  #
  # })
  #
  # output$probe_o <- renderUI ({
  #
  #   omegas <- probe_apply(beta_data())$time
  #
  #   idx <- seq(2, by = 2, length.out = length(omegas)/2)
  #
  #   textInput("in_o", "Specify A ends(s)",
  #             value = paste(omegas[idx], collapse = ","))
  #
  # })
  #
  # a_data_probe <- reactive ({
  #
  #   # shiny specific function
  #   wave_label_a_probe <- function (data, a, o) {
  #
  #     times <- unlist(seqvec(from = a, to = o, by = 0.01))
  #
  #     out <- data %>%
  #       mutate(waveform = ifelse(round(time, 2) %in% round(times, 2), "a", NA_character_))
  #
  #     return(out)
  #   }
  #
  #   a <- as.double(str_split(input$in_a, pattern = ",")[[1]])
  #   o <- as.double(str_split(input$in_o, pattern = ",")[[1]])
  #
  #   out <- wave_label_a_probe(beta_data(), a, o)
  #
  #   return(out)
  # })
  #
  # auto_data_probe <- reactive ({
  #
  #   udat <- a_data_probe()
  #
  #   out <- probe_comb(udat, e_var = as.double(input$in_evar_p))
  # })
  #
  # plot_probe <- reactive ({
  #
  #   if (input$adone_p == "n") {
  #     plot_wave(a_data_probe())
  #   }
  #
  #   else if (input$adone_p == "y") {
  #
  #     plot_wave(auto_data_probe())
  #   }
  #
  # })
  #
  # output$plot_probe <- renderPlotly ({
  #
  #   ggplotly(plot_probe())
  # })
  #
  # output$e_var_p <- renderUI ({
  #   textInput("in_evar_p", "Specify acceptable E Variance", value = 0.1)
  # })


})
