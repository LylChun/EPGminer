library(data.table)
library(dplyr)
library(epgminer)
library(htmlwidgets)
library(plotly)
library(readr)
library(shiny)
library(shinythemes)
library(stringr)
library(webshot)


options(shiny.maxRequestSize = 300*1024^2)

shinyUI(fluidPage(

  theme = shinytheme("paper"),

  titlePanel("EPGminer"),

  tabsetPanel(

    tabPanel("Welcome", fluid = TRUE,
             fluidRow(
               column(12, wellPanel(h3("Welcome to EPGminer!")))
             ),
             fluidRow(
               column(12,
                      wellPanel(
                        h4("Introduction"),
                        p("EPGminer is a web application designed to assist researchers in the
                          analysis of Electrical Penetration Graph (EPG) data. EPG data can help
                          scientists to better understand insect feeding behavior and host-insect
                          relationships."),
                        p("There are three parts to EPGminer: (i) Labeling raw data, (ii) Analysis
                          of labeled data through metric calculations, and (iii) Visualizations
                          of labeled data. A researcher may upload their data, label it, and then
                          analyze the data, all within a point and click interface that is user
                          friendly and requires no coding experience."),
                        p("One of the main functionalities of EPGminer is to allow researchers to
                          calculate frequencies for labeled waveforms using the Fourier Transform.
                          Additional waveform metrics are available as well, such as duration and
                          count. The other main functionality of EPGminer is the ability to generate
                          visuals. There are currently three visuals that EPGminer can create - a
                          frequency boxplot, a pie chart of waveform times, and a pie chart of
                          waveform counts. Each of these plots is customizable - e.g. the user may
                          select which waveforms to include in the plot itself. The plots are also
                          interactive and the user may zoom in, or 'hover' over the plot for
                          additional information."))
               )
             ),
             fluidRow(
               column(12,
                      wellPanel(
                        h4("Why use EPGminer?"),
                        p("EPGminer allows one to dig deeper into EPG data and calculate metrics that
                          require computational power, such as frequency. By providing the data analysis
                          capabilities of R code in a user-friendly manner, we hope to advance EPG
                          data analysis further. For example, the metrics calculated can be downloaded
                          and used to cross compare waveform types, differences both within and between
                          datasets, and even differences between insect/host combinations. All analysis
                          - both metrics and visuals, can be downloaded by the user.")
                      ))
             )),

    tabPanel("Tutorial", fluid = TRUE,
             fluidRow(
               column(12, wellPanel(h3("How to Use EPGminer")))
             ),
             fluidRow(
               column(12,
                      wellPanel(
                        h4("Quick Start Guide:"),
                        p("1. To begin, please click the 'Label My Data' tab, and
                          upload your data file(s). If the raw data is in separate hourly txt files,
                          highlight all of them when you click 'Browse'. In order to analyze your data,
                          you must upload both the raw voltage data and the annotation file for that data.
                          Alternatively, you may upload a single csv containing both the raw data and
                          the waveform labels. See 'Additional Resources' for more details on supported data
                          formats. If you choose 'Use Algorithm to Label' under 'Label My Data', only raw
                          data is required, no annotation file is necessary. For details on how to use this
                          option, see 'Additional Resources' and select 'Algorithmic Labeling'. Please
                          note that the algorithm is still in development and may not return accurate
                          results."),
                        p("2. When you have finished loading/labeling your data, proceed to the
                          'Analyze My Data' tab for analysis of the labeled waveforms. This tab
                          allows one to calculate metrics - frequency, duration, and count, as well as
                          to summarise the metric by waveform (frequency and duration only)."),
                        p("3. If you wish to visualize your data, you may select the 'Visuals' tab.
                          All visuals allow the user to select which waveform(s) to include. They are
                          also all interactive."),
                        p("4. If you encounter errors, or are unsure how to proceed, check out
                          'Tips and Troubleshooting' and/or 'Additional Resources'."))
               )
             ),
             fluidRow(
               column(12,
                      wellPanel(
                        h4("Tips and Troubleshooting:"),
                        p("Please note that all steps take some time. Because EPG data is collected
                          for many hours at a time and can thus be millions of rows when combined,
                          uploading data is not instantaneous. Allow a minimum of one minute
                          before checking that all inputs are correct. You may also see a
                          notification in the bottom right corner that says 'Rendering'. This
                          means the program is running and the output will be displayed when
                          finished."),
                        p("If you are adding Manual labels/annotation from an ANA file, ensure that
                          the raw data and the ANA file match and are for the same dataset. For example,
                          if there are 24 hours of data, you must upload the raw data for all 24 hours.
                          The file names do not need to match, only the contents."),
                        p("If the pop-up in the bottom right hand corner says 'Rendering', this is a
                          good indication that the program is running and will display the output
                          once finished."))
               )
             ),
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
                   p("c. ANA files must have the first two columns be waveform then time. Any
                     additional columns will not be used."),
                   p("b. Pre-labeled data must be in csv format with columns labeled
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
                     files and the ANA file match exactly. Also make sure that ALL raw data file(s)
                     have been uploaded - e.g if there are 24 hourly txt files corresponding to
                     this dataset, you must upload all 24.")
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
                     the bottom of the table when viewing the frequency or duration of each
                     individual waveform instance. The subforms labels, pd1 and pd2, will appear
                     in order with the other waveforms, but due to the way 'pd' unseparated into
                     subforms is handled by the app, they will appear all together on the bottom
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
                     the annotation for your data, it is recommended to use that instead.")),
                   p("Start by checking the A start(s)/end(s). To minimize sources of error, only
                     choose 'Yes' for multiple feeding starts IF you see multiple feeding beginnings
                     in your data. Once A has been labelled to your satisfaction, click Yes to the
                     'Are you finished labeling A?' input. The rest of the waveforms will be now be
                     algorithmically labelled. If at the end of this process you encounter an
                     error or E is present and has not been labelled, try to adjust the E variance
                     - this adjusts the amount of variation allowed in E. A larger
                     input value means a greater standard deviation is allowable - looser
                     parameters."),
                   p("Additionally if you wish to add pds after the algorithm has run, simple
                     click on the plot at the starts and ends of the pd(s) you wish to add.
                     The order of points does not matter, as they will be automatically re-ordered.
                     Then choose yes for 'Add Manually identified pds?'. Currently, pd subforms
                     are not supported."),
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
                              choices = c(`Add Manual Labels from ANA file` = "ana",
                                          `Pre-labeled Data` = "prelab",
                                          `Use Algorithm to Label` = "comp")),

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
                   fileInput("data", "Choose Labelled Data File",
                             multiple = FALSE, accept = ".csv")
                 ),

                 conditionalPanel(
                   condition = "input.label == 'comp'",
                   fileInput("compraw", "Choose Raw Data File(s)",
                             multiple = TRUE, accept = ".txt"),
                   radioButtons("probe", "Are there multiple feeding beginnings?",
                                choices = c(`No` = "n", `Yes` = "y")),
                   uiOutput("ao"),
                   radioButtons("adone", "Are you finished labeling A?",
                                choices = c(`No` = "n", `Yes` = "y")),
                   conditionalPanel(
                     condition = "input.adone == 'y'",
                     uiOutput("e_var"),
                     # could add other selectables
                     radioButtons("pd_manual", "Add manually identified pds?",
                                  choices = c(`No` = "n", `Yes` = "y")),
                     downloadButton("downloadcomp", "Download")
                   )
                 )

               ),

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
                   plotlyOutput("comp_plot"),

                   conditionalPanel(
                     condition = "input.adone == 'y'",
                     fluidRow(
                       column(6, tableOutput("comp_table")),
                       column(6, actionButton(inputId = "clear", "Clear selected points"))
                     ),
                     tableOutput("tab"))
                 )

               )

             )
    ),

    tabPanel("Analyze My Data", fluid = TRUE,

             sidebarLayout(

               sidebarPanel(

                 selectInput("metric", "Choose Desired Metric",
                             choices = c(`Frequency` = 'freq',
                                         `Duration` = 'dur',
                                         `Count` = "count")),
                 conditionalPanel(
                   condition = "input.metric == 'freq'",
                   radioButtons("summary", "Choose Summary Type",
                                choices = c(`Individual` = 'default',
                                            `Mean per Waveform` = "mean",
                                            `Median per Wavefrom` = "median",
                                            `SD within each Waveform` = "sd"))
                 ),
                 conditionalPanel(
                   condition = "input.metric == 'dur'",
                   radioButtons("summaryd", "Choose Summary Type",
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
                              choices = c(`Frequency Bar Chart` = 'fbar',
                                          `Pie chart of Waveforms` = "pie")),
                 conditionalPanel(
                   condition = "input.plottype == 'fbar'",
                   checkboxGroupInput("fbar_waves", "Choose Waveforms to Include",
                                      choices = c("A", "C", "E1", "E2", "G", "pd1", "pd2", "pd"),
                                      selected = c("E1", "E2", "G", "pd1", "pd2"))
                 ),
                 conditionalPanel(
                   condition = "input.plottype == 'pie'",
                   radioButtons("pietype", "Choose Type of Pie Chart",
                                choices = c(`By Time` = "pie_t",
                                            `By Count` = "pie_c")),
                   checkboxGroupInput("pie_waves", "Choose Waveforms to Include",
                                      choices = c("A", "C", "E1", "E2", "G", "pd1", "pd2", "pd"),
                                      selected = c("C", "E1", "E2", "G", "pd1", "pd2"))
                 ),
                 downloadButton(outputId = "pdf", label = "pdf"),
                 downloadButton(outputId = "png", label = "png"),
                 # downloadButton(outputId = "eps", label = "eps")
               ),

               mainPanel(plotlyOutput("plot"))
             )
    )
    ,

    tabPanel("Beta Zone", fluid = TRUE,

             sidebarLayout(

               sidebarPanel(

                 fileInput("beta", "Choose Raw Data Files",
                           multiple = TRUE, accept = ".txt"),
                 uiOutput("probe_a"),
                 uiOutput("probe_o"),
                 radioButtons("adone_p", "Are you finished labeling A?",
                              choices = c(`No` = "n", `Yes` = "y")),
                 conditionalPanel(
                   condition = "input.adone_p == 'y'",
                   uiOutput("e_var_p"),
                 )
               ),
               mainPanel(
                 DT::dataTableOutput("data_probe"),
                 plotlyOutput("plot_probe")
               ))
    )


  )

))
