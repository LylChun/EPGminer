#' Read in EPG data files
#'
#' @description The function read_epg data reads in EPG data.
#'
#' @usage read_epg(data, extension = c("txt", "ANA", "csv"))
#'
#' @param data As with other read_x functions, insert the file
#' path for the data file here. If there are multiple files, they must
#' be read in one at a time such as by using lapply.
#'
#' @param extension The file extension for the raw data that is to be read. The
#' options are txt, ANA, and csv. If no extension is indicated,
#' txt will be chosen by default.
#'
#' @details Three file formats are supported: raw data text files with format
#' time;volts, ANA annotation files with columns in order waveform, time,
#' and volts (they do not need to be named), and lastly csv files of combined
#' raw data and annotations with columns labeled time, volts, and waveform.
#'
#' @return A tibble object corresponding to the type of data input is returned.
#' @export
#'


read_epg <- function (data, extension = c("txt", "ANA", "csv")) {

  time = volts = NULL
  rm(list = c("time", "volts"))

  ext = match.arg(extension)

  if (ext == "txt") {
    out <- readr::read_lines(data) %>%
      # split into time and volts
      stringr::str_split_fixed(pattern = ";", n = 2)
    # add time and volts column names
    colnames(out) <- c("time", "volts")
    # turn into tibble
    out <- tibble::as_tibble(out) %>%
      # change data types into numeric
      dplyr::transmute(time = as.numeric(time),
                       volts = as.numeric(volts))
  }

  else if (ext == "ANA") {
    # read in ana file
    ana <- utils::read.csv(data, fileEncoding = "UTF-16", sep = "\t", header = FALSE)
    colnames(ana) <- c("waveform", "time", "volts")

    if (ana$time[1] != 0.00) {
      ana$time[1] <- 0.0
    }
    # return only waveform and time
    out <- ana[, 1:2]
  }

  else if (ext == "csv") {
    out <- readr::read_csv(data, col_names = TRUE, col_types = readr::cols_only(
      time = readr::col_double(), volts = readr::col_double(),
      waveform = readr::col_character()))
  }

  return(out)
}

