# library(data.table)
library(dplyr)
library(epgminer)
# library(htmlwidgets)
# library(plotly)
### library(readr)
# library(shiny)
# library(shinythemes)
# library(stringr)
# library(webshot)


options(shiny.maxRequestSize = 300*1024^2)

shinyUI(fluidPage(

  theme = shinythemes::shinytheme("paper"),

  tags$head(tags$title("EPGminer")),

  # titlePanel("EPGminer"),
  titlePanel(title = div(img(src = "logo.png", width = "5%", height = "5%",
                             align = "center"),
                         "EPGminer")),

  tabsetPanel(

    tabPanel("Welcome", fluid = TRUE,
             fluidRow(
               column(12, wellPanel(h3("Welcome to EPGminer!")))
             ),
             fluidRow(
               column(6,
                      wellPanel(
                        h4("Overview"),
                        p("EPGminer is an application designed to help analyze Electrical
                          Penetration Graph (EPG) data. EPG data can help scientists better
                          understand insect feeding behavior and host-insect relationships.
                          Since EPG datasets can often be large and difficult to analyze, there
                          is a need for user friendly software that can assist in that analysis.
                          The EPGminer app is based on the epgminer R package which can be found on
                          Github at LylChun/EPGminer"),
                        p("There are three parts to EPGminer:"),
                        p("(i) Data upload and labeling"),
                        p("(ii) Data Analysis using Frequency and Relative Amplitude"),
                        p("(iii) Data Visualizations via interactive plots")
                      ),
                      wellPanel(
                        h4("Why use EPGminer?"),
                        p("EPGminer allows one to dig deeper into EPG data and calculate metrics that
                          require computational power, such as frequency. By providing the data analysis
                          capabilities of R code in a user-friendly manner, we hope to advance EPG
                          data analysis further. Additionally, all analyses - both metrics and visuals,
                          can be downloaded by the user for further study.")
                        # p("Additionally, EPGminer includes an experimental semi-automatic algorithm to label
                        #   waveforms in raw EPG data. Manual annotations of EPG data are time-consuming
                        #   and a computer mediated method of annotating is desireable. In this
                        #   semi-automatic algorithm, we have sought to balance the benefits of automatic
                        #   labeling with the need for accuracy. The automatic portion allows one to save
                        #   time and energy by computationally labeling waveforms. Then the user input
                        #   features - making it 'semi'-automatic - can be used to fine-tune the
                        #   computer labeling to improve accuracy. The algorithm is still under
                        #   development, so if you already possess manual annotations (ANA files) for
                        #   your data, it is recommended that you use those instead for best accuracy.
                        #   However if you do not have manual annotations, or you simply would like to
                        #   see the algorithm in action, then the option is available.")
                      )
               ),
               column(6,
                      fluidRow(
                        column(6,
                               "Example Pie Chart of Waveforms by Time",
                               img(src = "pie_time.png", height = "100%", width = "100%%"),
                        ),
                        column(6,
                               "Example Pie Chart of Waveforms by Number",
                               img(src = "pie_number.png", height = "100%", width = "100%%")
                        )
                      ),
                      "Example Boxplot of Frequencies",
                      img(src = "freq_boxplot.png", width = "100%")
               )

             ),
    ),

    tabPanel("Tutorial", fluid = TRUE,
             fluidRow(
               column(12, wellPanel(h3("Getting Started with EPGminer")))
             ),
             fluidRow(
               column(6,
                      wellPanel(
                        h4("Quick Start Guide:"),
                        p("1. Click the 'Label My Data' tab and upload your data file(s). For
                        specifics on supported data types, please see 'Additional Resources' below."),
                        p("2. Once the data has loaded, proceed to the 'Analyze My Data' tab for
                        frequency and relative amplitude calculations, or the 'Visuals' tab for
                        visualizations of your data."),
                        p("3. If you encounter errors, or are unsure how to proceed, check out
                          'Tips and Troubleshooting' and/or 'Additional Resources'."))
               ),
               column(6,
                      wellPanel(
                        h4("Tips and Troubleshooting:"),
                        p("Please note that all steps, particularly data upload/labeling and visuals,
                        can take a nontrivial amount of time. "),
                        p("When uploading data,", strong("please allow a minimum of one minute"), "
                        before checking that all inputs are correct. You may also see a notification
                        in the bottom right corner that says 'Rendering' while the program is running."),
                        p("Algorithmic labeling is currently experimental and prone to errors.
                          It is", strong("strongly recommended"), "to use manual labels (ANA)
                          or prelabeled data instead."))
               )
             ),
             # fluidRow(
             #   column(12,
             #          wellPanel(
             #            h4("Tips and Troubleshooting:"),
             #            p("Please note that all steps take some time. Because EPG data is collected
             #              for many hours at a time and can thus be millions of rows when combined,
             #              uploading data is not instantaneous. Allow a minimum of one minute
             #              before checking that all inputs are correct. You may also see a
             #              notification in the bottom right corner that says 'Rendering'. This
             #              means the program is running and the output will be displayed when
             #              finished. Additionally, if you see an error displaying, but the 'Rendering'
             #              banner is also displayed, wait until the Rendering banner disappears as
             #              the error message may be old/transient and disappear once the program
             #              finishs rendering."),
             #            p("If you are adding Manual labels/annotation from an ANA file, ensure that
             #              the raw data and the ANA file match and are for the same dataset. For example,
             #              if there are 24 hours of data, you must upload the raw data for all 24 hours.
             #              The file names do not need to match, only the contents."),
             #            p("The algorithm for labeling data is still under active development and prone
             #              to erroring out or mislabeling. For detailed instructions on how to use it
             #              and best avoid errors, please see the 'Additional Resources' section on
             #              'Algorithmic Labeling'."))
             #   )
             # ),
             fluidRow(
               column(12, wellPanel(
                 h4("Additional Resources"),
                 selectInput("help", "Choose Topic",
                             choices = c(`Make Selection` = "none",
                                         `Data Upload/Labeling` = "upload",
                                         `Data Analysis` = "analysis",
                                         `Visuals` = "visuals",
                                         `Algorithmic Labeling` = "comp")),
                 conditionalPanel(
                   condition = "input.help == 'upload'",
                   p("Supported data formats are as follows:"),
                   p("a. Raw data has two acceptable formats, please select the Raw Data
                          File Type that best suits your needs. Raw data in .txt format may
                          be multiple txt files (please highlight/select all). The txt file must
                          have the format time;volts. Raw data in .csv format must be a
                          single csv file with columns labeled time, volts."),
                   p("b. ANA files must have the first two columns be waveform then time. Any
                     additional columns will not be used."),
                   p("c. Pre-labeled data must be in csv format with columns labeled
                     time, volts, and waveform."),
                   p("If you encounter an issue or error when uploading/labeling your data, first
                     check that all data file(s) are properly formatted. If all files are properly
                     formatted but you do not see an output, please wait for at least a minute to allow
                     time to read in/process the data properly. If at the end of that time,
                     there still is no output, or you have encountered an error, consider the
                     following:"),
                   p("(i) Make sure that your uploaded data type matches with the selected option. If
                     the 'Choose Raw Data File Type' input does not match with what you have uploaded,
                     please change the input and try again."),
                   p("(ii) For the 'Add Manual Labels from ANA' option, check that the raw data
                     files and ANA are from the same dataset.")
                 ),
                 conditionalPanel(
                   condition = "input.help == 'analysis'",
                   p("First make sure the data is properly uploaded by checking the 'Upload
                     My Data' tab and ensuring that the first five rows are diplayed. If
                     you are using algorithmically labeled data, make sure the plot displays,
                     properly labeled before proceeding. If the data is displayed as expected,
                     then return to the 'Analyze My Data' tab and wait an additional 30 seconds
                     to allow time to process."),
                   p("For the waveform annotation 'pd', all calculated results will appear at
                     the bottom of the table when viewing the frequency or relative amplitude of each
                     individual waveform instance. The subforms labels, pd1 and pd2, will appear
                     in order with the other waveforms, but due to the way 'pd' unseparated into
                     subforms is handled, they will appear all together on the bottom
                     of the table.")
                 ),
                 conditionalPanel(
                   condition = "input.help == 'visuals'",
                   p("Visuals may take some time to render on your machine. If after waiting
                     at least 20 seconds you still do not see an output, check your data
                     upload and make sure all is correct. (See 'Data Upload/Labeling' and
                     'Data Analysis')."),
                   p("Downloading is not instantaneous. Please only click the download button once.
                     Your download will begin as soon as you click, and clicking multiple times will
                     result in multiple downloads."),
                   p("Downloads of visuals will represent the 'base' state - e.g. if the user
                     has selected only waveform G, then the downloaded plot will only include G.
                     However, any 'zoom' or 'hover' information will not be included. Thus if you
                     zoom into a particular region, the downloaded plot will still include the
                     entire, unzoomed, plot. Additionally, although the plots are interactive
                     within the app, they will download as pdf/png files which are not interactive.")
                 ),
                 conditionalPanel(
                   condition = "input.help == 'comp'",
                   p(strong("The algorithm is currently experimental and prone to error. If you have
                     the annotation for your data, it is strongly recommended to use that instead.")),
                   p("Start by determining if your data has multiple feeding beginnings. If the insect
                     has retracted the probe and reinserted it, as seen in the plotted voltage data,
                     then select yes for 'Multiple Feeding Beginning'. To minimize error, only select
                     yes if there are truly multiple feeding beginnings."),
                   p("Then begin the labeling by checking the A start(s)/end(s). Once A has been
                     labeled to your satisfaction, click Yes to the 'Are you finished labeling A?'
                     input. The rest of the waveforms will be now be algorithmically labelled. If
                     at the end of this process you encounter an error or E is present and has
                     not been labelled, try to adjust the E variance - this adjusts the amount of
                     variation allowed in E. A larger input value means a greater standard deviation
                     is allowable - looser parameters that increase chances of finding E. Similarly
                     for G, if G is present and has not been found, try adjusting the 'Specify
                     acceptable G drop' input. Note that the input value is simply a multiplier
                     used in the algorithm and does not represent absolute voltage values. A larger
                     input value here will allow a larger drop from the pre-feeding baseline."),
                   p("Additionally if you wish to add pds after the algorithm has run, simple
                     click on the plot at the starts and ends of the pd(s) you wish to add.
                     The order of points does not matter, as they will be automatically re-ordered.
                     Then choose yes for 'Add Manually identified pds?'. Currently, this option
                     is only available for data with only one feeding beginning."),
                   p("Only click 'Clear Selected Points' if you have made an error in choosing
                     pd starts/ends. The selected points are displayed on the bottom left just
                     below the plot. Please note that clicking this button will clear ALL selected
                     points. If you have not already added the points by selecting 'Yes' for 'Add
                     Manually identified pds?', then double-clicking on the plot to 'unzoom' will
                     have the same effect - ALL selected points will be wiped. It is recommended
                     to instead use the zoom in/out buttons on the top right of the plot. You may
                     double click to unzoom once you have clicked 'Yes' for adding manual pds."),
                   p("Do not click the download button more than once. Writing the data to a csv
                     file and downloading will take time so you will not see the download right away,
                     but once the download button has been pressed, your download will automatically
                     begin. The downloaded csv file can now be used in EPGminer as 'Pre-labeled'
                     data if you so chose.")
                 ),
                 p("For specific bugs/errors or suggestions, feel free to reach out:
                   lizzie_chun1 [at] tamu.edu")
               ))
             )),

    tabPanel("Label My Data", fluid = TRUE,

             sidebarLayout(

               sidebarPanel(
                 radioButtons("label", "Choose Labeling Method",
                              choices = c(`Manual Labels from ANA file` = "ana",
                                          `Pre-labeled Data` = "prelab",
                                          `Algorithmic Labeling (BETA)` = "comp")),

                 conditionalPanel(
                   condition = "input.label == 'ana'",
                   selectInput("rawtype", "Choose Raw Data File Type",
                               choices = c(`.txt (multiple files allowed)` = "txt",
                                           `.csv (single file)` = "csv")),
                   conditionalPanel(
                     condition = "input.rawtype == 'txt'",
                     fileInput("rawdata2", "Choose Raw Data File(s)",
                               multiple = TRUE, accept = ".txt")
                   ),
                   conditionalPanel(
                     condition = "input.rawtype == 'csv'",
                     fileInput("rawdata", "Choose Raw Data File",
                               multiple = FALSE, accept = ".csv")
                   ),
                   fileInput("anadata", "Choose ANA Label File",
                             multiple = FALSE, accept = ".ANA")
                 ),

                 conditionalPanel(
                   condition = "input.label == 'prelab'",
                   fileInput("data", "Choose Labeled Data File",
                             multiple = FALSE, accept = ".csv")
                 ),

                 conditionalPanel(
                   condition = "input.label == 'comp'",
                   fileInput("compraw", "Choose Raw Data File(s)",
                             multiple = TRUE, accept = ".txt"),
                   selectInput("probe", "Are there multiple feeding beginnings?",
                               choices = c(`Select Option` = "blank", `No` = "n",
                                           `Yes` = "y")),

                   conditionalPanel(
                     condition = "input.probe == 'n'",
                     uiOutput("ao"),
                     radioButtons("adone", "Are you finished labeling A?",
                                  choices = c(`No` = "n", `Yes` = "y")),

                     conditionalPanel(
                       condition = "input.adone == 'y'",
                       uiOutput("e_var"),
                       uiOutput("g_drop"),
                       # could add other selectables
                       radioButtons("pd_manual", "Add manually identified pds?",
                                    choices = c(`No` = "n", `Yes` = "y")),
                       downloadButton("downloadcomp", "Download")
                     )
                   ),

                   conditionalPanel(
                     condition = "input.probe == 'y'",
                     uiOutput("probe_a"),
                     uiOutput("probe_o"),
                     radioButtons("adone_p", "Are you finished labeling A?",
                                  choices = c(`No` = "n", `Yes` = "y")),
                     conditionalPanel(
                       condition = "input.adone_p == 'y'",
                       uiOutput("e_var_p"),
                       uiOutput("g_drop_p"),
                       downloadButton("downloadcomp_probe", "Download")
                     )
                   )
                 )),

               mainPanel(

                 conditionalPanel(
                   condition = "input.label == 'ana' || input.label == 'prelab'",

                   fluidRow(
                     column(12,
                            p("First five rows of data for reference (after loading):"))
                   ),
                   DT::dataTableOutput("data")
                 ),

                 conditionalPanel(
                   condition = "input.label == 'comp'",

                   conditionalPanel(
                     condition = "input.probe == 'blank'",
                     plotly::plotlyOutput("vts_plot")
                   ),

                   conditionalPanel(
                     condition = "input.probe == 'n'",
                     plotly::plotlyOutput("comp_plot"),

                     conditionalPanel(
                       condition = "input.adone == 'y'",
                       fluidRow(
                         column(6, tableOutput("comp_table")),
                         column(6, actionButton(inputId = "clear", "Clear selected points"))
                       ),
                       tableOutput("tab"))
                   ),

                   conditionalPanel(
                     condition = "input.probe == 'y'",

                     plotly::plotlyOutput("plot_probe")
                   )
                 )
               )
             )
    ),

    tabPanel("Analyze My Data", fluid = TRUE,

             sidebarLayout(

               sidebarPanel(

                 selectInput("metric", "Choose Desired Metric",
                             choices = c(`Frequency` = 'freq',
                                         # `Duration` = 'dur',
                                         # `Occurrence` = 'count',
                                         # `Mean Volts` = 'mean_volts',
                                         # `SD volts` = 'sd_volts',
                                         `Relative Amplitude (volts)` = 'amp_volts')),
                 conditionalPanel(
                   condition = "input.metric == 'freq'",
                   radioButtons("summary", "Choose Summary Type",
                                choices = c(`Individual` = 'default',
                                            `Median per Wavefrom` = "median",
                                            `Mean per Waveform` = "mean",
                                            `SD within each Waveform` = "sd"))
                 ),
                 conditionalPanel(
                   condition = "input.metric == 'dur'",
                   radioButtons("summaryd", "Choose Summary Type",
                                choices = c(`Individual` = 'default',
                                            `Median per Wavefrom` = "median",
                                            `Mean per Waveform` = "mean",
                                            `SD within each Waveform` = "sd"))
                 ),
                 conditionalPanel(
                   condition = "input.metric == 'mean_volts'",
                   radioButtons("summarymv", "Choose Summary Type",
                                choices = c(`Individual` = 'default',
                                            `Median per Wavefrom` = "median",
                                            `Mean per Waveform` = "mean",
                                            `SD within each Waveform` = "sd"))
                 ),
                 conditionalPanel(
                   condition = "input.metric == 'sd_volts'",
                   radioButtons("summarysv", "Choose Summary Type",
                                choices = c(`Individual` = 'default',
                                            `Median per Wavefrom` = "median",
                                            `Mean per Waveform` = "mean",
                                            `SD within each Waveform` = "sd"))
                 ),
                 conditionalPanel(
                   condition = "input.metric == 'amp_volts'",
                   radioButtons("summaryav", "Choose Summary Type",
                                choices = c(`Individual` = 'default',
                                            `Median per Wavefrom` = "median",
                                            `Mean per Waveform` = "mean",
                                            `SD within each Waveform` = "sd"))
                 )
               ),

               mainPanel(DT::dataTableOutput("metric"))

             )
    ),

    tabPanel("Visuals", fluid = TRUE,

             sidebarLayout(

               sidebarPanel(

                 radioButtons("plottype", "Choose Desired Visualization",
                              choices = c(`Frequency Box Plot` = 'fbox',
                                          `Pie Chart of Waveforms` = "pie",
                                          `Labeled Time Series` = "wave")),
                 conditionalPanel(
                   condition = "input.plottype == 'fbox'",
                   checkboxGroupInput("fbox_waves", "Choose Waveforms to Include",
                                      choices = c("non-probing", "C", "E1", "E2", "G", "pd1", "pd2", "pd"),
                                      selected = c("E1", "E2", "G", "pd1", "pd2"))
                 ),
                 conditionalPanel(
                   condition = "input.plottype == 'pie'",
                   radioButtons("pietype", "Choose Type of Pie Chart",
                                choices = c(`By Time` = "pie_t",
                                            `By Number` = "pie_c")),
                   checkboxGroupInput("pie_waves", "Choose Waveforms to Include",
                                      choices = c("non-probing", "C", "E1", "E2", "G", "pd1", "pd2", "pd"),
                                      selected = c("E1", "E2", "G", "pd1", "pd2"))
                 ),
                 downloadButton(outputId = "pdf", label = "pdf"),
                 downloadButton(outputId = "png", label = "png"),
                 # downloadButton(outputId = "eps", label = "eps")
               ),

               mainPanel(plotly::plotlyOutput("plot"))
             )
    )
  )

))
