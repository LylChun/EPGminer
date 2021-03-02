library(data.table)
library(dplyr)
library(epgminer)
library(plotly)
library(readr)
library(shiny)
library(shinythemes)
library(stringr)

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
                        h4("How to's:"),
                        p("1. To get started, please click the 'Label My Data' tab, and
                          upload your data file(s)."),
                        p("2. When you have finished loading/labeling your data, proceed to the
                          'Analyze My Data' tab for analysis of the labelled waveforms"),
                        p("3. If you wish to visualize your data, you may select
                          the 'Visuals' tab."))
               )
             ),
             fluidRow(
               column(12,
                      wellPanel(
                        h4("Tips and Troubleshooting:"),
                        p("Please note that all steps take some time. Allow a minimum of 20 seconds
                          before checking that all inputs are correct. You may also see a
                          notification in the bottom right corner that says 'Rendering'. This
                          means the program is running and the output will be displayed when
                          finished."),
                        p("Uploaded data must be formatted as follows:"),
                        p("a. Pre-labelled data must be in csv format with columns labeled
                          time, volts, and waveform "),
                        p("b. Raw data has two acceptable formats, please select the Raw Data
                          File Type that best suits your needs. Raw data in .csv format must
                          be a single csv file with columns labeled time, volts. Raw data in
                          .txt format may be multiple txt files (please highlight/select all).
                          The txt file must have the format time;volts."),
                        p("c. ANA files must have the first two columns be waveform then time."))
               )
             ),
             fluidRow(
               column(12, wellPanel(
                 h4("Additional Help/Resources"),
                 selectInput("help", "Choose Topic",
                             choices = c(`Make Selection` = "none",
                                         `Data Upload` = "upload",
                                         `Data Analysis` = "analysis",
                                         `Visuals` = "visuals",
                                         `Algorithmic Labelling` = "comp")),
                 conditionalPanel(
                   condition = "input.help == 'upload'",
                   p("First check that all data file(s) are properly formatted. (See Tips
                     and Troubleshooting section above). If all files are properly formatted
                     but you do not see an output, please wait for at least a minute to allow
                     time to read in/process the data properly. If at the end of that time,
                     there still is no output, or you have encountered an error, consider the
                     following:"),
                   p("For the 'Add Manual Labels from ANA' option, check that the raw data
                     files and the ANA file match exactly. Also make sure that ALL raw data file(s)
                     have been uploaded - e.g if there are 24 hourly txt files corresponding to
                     this dataset, you must upload all 24.")
                 ),
                 conditionalPanel(
                   condition = "input.help == 'analysis'",
                   p("First make sure the data is properly uploaded by checking the 'Upload
                     My Data' tab and ensuring that the first five rows are diplayed. If
                     you are using algorithmically labelled data, make sure the plot displays
                     properly labelled before proceeding. If the data is displayed as expected,
                     then return to the 'Analyze My Data' tab and wait at least 30 seconds
                     to allow time to process.")
                 ),
                 conditionalPanel(
                   condition = "input.help == 'visuals'",
                   p("Visuals may take some time to render on your machine. If after waiting
                     at least 20 seconds you still do not see an output, check your data
                     upload and make sure all is correct. (See 'Data Upload' and 'Data Analysis').")
                 ),
                 conditionalPanel(
                   condition = "input.help == 'comp'",
                   p("Start by checking the A start/end. Currently the algorithm only supports
                     a single instance of waveform A - feeding activity must begin only once.
                     Once A has been labelled to your satisfaction, click Yes to the 'Are you
                     finished labeling A?' input. The rest of the waveforms will be now be
                     algorithmically labelled. If at the end of this process you encounter an
                     error or E is present and has not been labelled, try to adjust the E variance
                     - this adjusts the amount of variation allowed in E. A larger
                     e_var value means a greater standard deviation is allowable - looser
                     parameters."),
                   p("Additionally if you wish to add pds after the algorithm has run, simple
                     click on the plot at the starts and ends of the pd(s) you wish to add.
                     The order of points does not matter, as they will be automatically re-ordered.
                     Then choose yes for 'Add Manually identified pds?'. Currently, pd subforms
                     are not yet supported."),
                   p("Only click 'Clear Selected Points' if you have made an error in choosing
                     pd starts/ends. The selected points are displayed on the bottom left just
                     below the plot. Please note that clicking this button will clear ALL selected
                     points. If you have not already added the points by selecting 'Yes' for 'Add
                     Manually identified pds?', then double-clicking on the plot to 'unzoom' will
                     have the same effect - ALL selected points will be wiped. It is recommended
                     to instead use the zoom in/out buttons on the top right of the plot. You may
                     double click to unzoom once you have clicked 'Yes' for adding manual pds."),
                   p("Do not click the download button more than once. Writing the data to a csv
                     file and downloading will take time so you may not see the download right away,
                     but once the download button has been pressed, your download will automatically,
                     begin. The downloaded csv file can now be used in EPGminer as 'Pre-labelled'
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
                                          `Pre-labelled Data` = "prelab",
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
                   condition = "input.plottype == 'pie'",
                   radioButtons("pietype", "Choose Type of Pie Chart",
                                choices = c(`By Time` = "pie_t",
                                            `By Count` = "pie_c"))
                 ),
                 downloadButton(outputId = "pdf", label = "pdf"),
                 downloadButton(outputId = "png", label = "png"),
                 downloadButton(outputId = "eps", label = "eps")
               ),

               mainPanel(plotOutput("plot"))
             )
    )
    # ,
    #
    # tabPanel("Beta Zone", fluid = TRUE,
    #
    #          sidebarLayout(
    #
    #            sidebarPanel(
    #
    #              fileInput("beta", "Choose Raw Data Files",
    #                        multiple = TRUE, accept = ".txt"),
    #              uiOutput("probe_a"),
    #              uiOutput("probe_o"),
    #              radioButtons("adone_p", "Are you finished labeling A?",
    #                           choices = c(`No` = "n", `Yes` = "y")),
    #              conditionalPanel(
    #                condition = "input.adone_p == 'y'",
    #                uiOutput("e_var_p"),
    #              )
    #            ),
    #            mainPanel(
    #              DT::dataTableOutput("data_probe"),
    #              plotlyOutput("plot_probe")
    #            ))
    # )


  )

))
