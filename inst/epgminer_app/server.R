source("algorithm.R")

shinyServer(function(input, output, session) {

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
      out <- data.table::rbindlist(list)
      rm(list)
    }

    return(out)
  })

  comp_raw <- reactive ({

    validate (
      need (input$compraw, "Waiting on Data Upload...")
    )

    list <- lapply(input$compraw$datapath, read_epg)
    out <- data.table::rbindlist(list)
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

    else if (input$label == "comp") {

      validate(
        need(input$probe %in% c("n", "y"), message = FALSE)
      )

      if (input$probe == "n") {
        data = comp_label()
      }

      else if (input$probe == "y") {
        # if add option to manually annotate pds, will need to change this as well
        data = auto_data_probe()
      }
    }

    return(data)
  })

  output$data <- DT::renderDataTable({

    DT::datatable(analyze_data()[1:5, ],
                  options = list(dom = "t"),
                  rownames = FALSE)
  })

  ################### Analyze My Data ####################

  metric_data <- reactive ({

    if (input$metric == "freq") {
      out <- wave_topfreq(analyze_data())
    }

    else if (input$metric == "dur") {

      out <- wave_duration(analyze_data())
    }

    else if (input$metric == "count") {
      out <- wave_occurrence(analyze_data())
    }

    else if (input$metric %in% c("mean_volts", "sd_volts", "amp_volts")) {
      out <- wave_volts(analyze_data())
    }

    return(out)
  })

  metric_tab <- reactive ({

    out <- metric_data()

    if (input$metric == "freq") {

      if (input$summary == "default") {
        out <- out
        colnames(out) <- c("waveform", "frequency (Hz)")
      }

      else if (input$summary == "median") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    frequency = round(median(frequency), 2))
        colnames(out) <- c("waveform", "frequency (Hz)")
      }

      else if (input$summary == "mean") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    topfreq = round(mean(frequency), 2))
        colnames(out) <- c("waveform", "frequency (Hz)")
      }

      else if (input$summary == "sd") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    sd = round(sd(frequency), 2))
      }

    }

    else if (input$metric == "dur") {

      if (input$summaryd == "default") {
        out <- out
        colnames(out) <- c("waveform", "duration (seconds)")
      }

      else if (input$summaryd == "median") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    duration = round(median(duration), 2))
        colnames(out) <- c("waveform", "duration (seconds)")
      }

      else if (input$summaryd == "mean") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    duration = round(mean(duration), 2))
        colnames(out) <- c("waveform", "duration (seconds)")
      }

      else if (input$summaryd == "sd") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    sd = round(sd(duration), 2))
      }

    }

    else if (input$metric == "count") {
      out <- out
    }

    if (input$metric == "mean_volts") {

      out <- out %>%
        select(waveform, mean_volts)

      if (input$summarymv == "default") {
        out <- out
        colnames(out) <- c("waveform", "mean volts")
      }

      else if (input$summarymv == "median") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    mean_volts = round(median(mean_volts), 2))
        colnames(out) <- c("waveform", "mean volts")
      }

      else if (input$summarymv == "mean") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    mean_volts = round(mean(mean_volts), 2))
        colnames(out) <- c("waveform", "mean volts")
      }

      else if (input$summarymv == "sd") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    sd = round(sd(mean_volts), 2))
      }

    }

    if (input$metric == "sd_volts") {

      out <- out %>%
        select(waveform, sd_volts)

      if (input$summarysv == "default") {
        out <- out
        colnames(out) <- c("waveform", "SD volts")
      }

      else if (input$summarysv == "median") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    sd_volts = round(median(sd_volts), 2))
        colnames(out) <- c("waveform", "SD volts")
      }

      else if (input$summarysv == "mean") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    sd_volts = round(mean(sd_volts), 2))
        colnames(out) <- c("waveform", "SD volts")
      }

      else if (input$summarysv == "sd") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    sd = round(sd(sd_volts), 2))
      }

    }

    if (input$metric == "amp_volts") {

      out <- out %>%
        select(waveform, relative_amplitude)

      if (input$summaryav == "default") {
        out <- out
        colnames(out) <- c("waveform", "Relative Amplitude (Volts)")
      }

      else if (input$summaryav == "median") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    relative_amplitude = round(median(relative_amplitude), 2))
        colnames(out) <- c("waveform", "Relative Amplitude (Volts)")
      }

      else if (input$summaryav == "mean") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    relative_amplitude = round(mean(relative_amplitude), 2))
        colnames(out) <- c("waveform", "Relative Amplitude (Volts)")
      }

      else if (input$summaryav == "sd") {
        out <- out %>%
          group_by(waveform) %>%
          summarise(waveform = waveform[1],
                    sd = round(sd(relative_amplitude), 2))
      }

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

  plot_react <- reactive({

    plot_id <- showNotification("Rendering...", duration  = NULL, closeButton = FALSE)
    on.exit(removeNotification(plot_id), add = TRUE)

    if (input$plottype == "fbox") {

      gg <- plot_fbox(analyze_data(), waveforms = input$fbox_waves)

      plotly::ggplotly(gg)
    }

    else if (input$plottype == "pie") {

      if (input$pietype == "pie_t") {

        plot_pie(analyze_data(), pietype = "time", waveforms = input$pie_waves)
      }

      else if (input$pietype == "pie_c") {

        plot_pie(analyze_data(), pietype = "number", waveforms = input$pie_waves)
      }
    }

    else if (input$plottype == "wave") {
      p <- plot_wave(analyze_data(), aggregate = "all")
      plotly::ggplotly(p)
    }

  })

  output$plot <- renderPlotly({
    plot_react()
  })

  visual_name <- reactive({

    if (input$plottype == "fbox") {
      out <- "Frequency_boxplot"
    }

    else if (input$plottype == "pie") {
      out <- "Waveform_piechart"
    }

    else if (input$plottype == "wave") {
      out <- "Labeled_time-series"
    }

    return(out)
  })

  options(shiny.usecairo = TRUE)

  output$pdf <- downloadHandler(
    filename = function() {
      paste(visual_name(), '.pdf', sep = '')
    },
    content = function(file) {

      htmlwidgets::saveWidget(plotly::as_widget(plot_react()), "temp.html",
                              selfcontained = FALSE)
      webshot::webshot(url = "temp.html", file, cliprect = "viewport", zoom = 0.5)
    }

  )

  output$png <- downloadHandler(
    filename = function() {
      paste(visual_name(), '.png', sep = '')
    },
    content = function(file) {

      htmlwidgets::saveWidget(plotly::as_widget(plot_react()),
                              "temp.html", selfcontained = FALSE)
      webshot::webshot(url = "temp.html", file, cliprect = "viewport", zoom = 0.5)
    }
  )

  ######################## Algorithm ########################

  comp_raw <- reactive ({
    req(input$compraw)

    list <- lapply(input$compraw$datapath, read_epg)
    out <- rbindlist(list)
    return(out)
  })

  output$vts_plot <- renderPlotly({
    vts_id <- showNotification("Rendering...", duration  = NULL, closeButton = FALSE)
    on.exit(removeNotification(vts_id), add = TRUE)

    ggplotly(plot_vts(comp_raw()))
  })

  ############# Single probe instance ############

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

    eg <- wave_label_eg(a_data(), e_var = as.double(input$in_evar),
                        g_drop = as.double(input$in_gdrop))
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

    ggplotly(plot_comp())

  })

  output$ao <- renderUI ({

    req(input$probe == "n")

    ao <- a_ao(comp_raw(), a_o = c(0.75, 0.5, 1, 1.25), a_drop = 0.75)$time

    textInput("in_ao", "Specify A start and end times",
              value = paste(ao[1], ", ", ao[2], sep = ""))

  })

  output$e_var <- renderUI ({
    sliderInput("in_evar", "Specify acceptable E Variance", min = 0, max = 0.5,
                value = 0.2, step = 0.1)
  })

  output$g_drop <- renderUI ({
    textInput("in_gdrop", "Specify acceptable G drop from non-feeding baseline",
              value = 0.75)
  })

  values <- reactiveValues()

  observe ({
    req(input$in_ao)

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
      filename <- stringr::str_sub(input$compraw$name, end = -8)
      paste(filename, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(comp_label(), file, row.names = FALSE)
    }
  )

  ################## Multiple Probes ####################

  output$probe_a <- renderUI ({

    alphas <- probe_apply(comp_raw())$time

    idx <- seq(1, by = 2, length.out = length(alphas)/2)

    textInput("in_a", "Specify A start(s)",
              value = paste(alphas[idx], collapse = ","))

  })

  output$probe_o <- renderUI ({

    omegas <- probe_apply(comp_raw())$time

    idx <- seq(2, by = 2, length.out = length(omegas)/2)

    textInput("in_o", "Specify A ends(s)",
              value = paste(omegas[idx], collapse = ","))

  })

  a_data_probe <- reactive ({

    # shiny specific function
    wave_label_a_probe <- function (data, a, o) {

      times <- unlist(seqvec(from = a, to = o, by = 0.01))

      out <- data %>%
        mutate(waveform = ifelse(round(time, 2) %in% round(times, 2), "a", NA_character_))

      return(out)
    }

    a <- as.double(str_split(input$in_a, pattern = ",")[[1]])
    o <- as.double(str_split(input$in_o, pattern = ",")[[1]])

    out <- wave_label_a_probe(comp_raw(), a, o)

    return(out)
  })

  auto_data_probe <- reactive ({

    udat <- a_data_probe()

    out <- probe_comb(udat, e_var = as.double(input$in_evar_p),
                      g_drop = as.double(input$in_gdrop_p))
  })

  plot_probe <- reactive ({

    if (input$adone_p == "n") {
      plot_wave(a_data_probe())
    }

    else if (input$adone_p == "y") {

      validate (
        need (!is.null(input$in_evar_p), "Please Wait - Rendering")
      )

      plot_wave(auto_data_probe())
    }

  })

  output$plot_probe <- renderPlotly ({

    probe_id <- showNotification("Rendering...", duration  = NULL, closeButton = FALSE)
    on.exit(removeNotification(probe_id), add = TRUE)

    ggplotly(plot_probe())
  })

  output$e_var_p <- renderUI ({
    sliderInput("in_evar_p", "Specify acceptable E Variance", min = 0, max = 0.5,
                value = 0.2, step = 0.1)
  })

  output$g_drop_p <- renderUI ({
    textInput("in_gdrop_p", "Specify acceptable G drop from non-feeding baseline",
              value = 0.75)
  })

  output$downloadcomp_probe <- downloadHandler(
    filename = function() {
      filename <- stringr::str_sub(input$compraw$name, end = -8)
      paste(filename, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(auto_data_probe(), file, row.names = FALSE)
    }
  )

  session$onSessionEnded ( function () {

    if (file.exists("temp.html")) {

      unlink(c("temp_files", "temp.html"), recursive = TRUE)
    }

  })

})
