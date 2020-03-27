## Shiny app for TropFishR related data exploration
## ui script

## load packages
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
library(rmarkdown)
library(TropFishR)
library(htmltools)

## Load scripts
##-----------------------------------------------------------------------------------
source("../funcs/uiFuncs.R")


shinyUI(
    fluidPage(

        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(text = jscode, functions=c("closeWindow")),

        ## style settings
        tags$head(
                 tags$style(HTML("
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(11) {
                           float: right;
                           font-weight: bold;
                           color: black;
                           }

                           "))
             ),


        navbarPage(title = "ShinyTropFish",
                   id = "tabset",
                   theme = shinytheme("cerulean"),
                   ##                   position = "fixed-top",
                   inverse = TRUE,
                   collapsible = TRUE,


                   ##        tabsetPanel(id="tabset", type = "pills",
                   tabPanel("Home", id = "home",
                            ##-----------------------------------------------------------
                            ##                                        headerPanel(title = "TropFishS"),
                            tagList(
                                ##                                            h2("TropFishS"),h3(" - This app allows you to do a length-based stock assessment without requiring any R knowledge."),

                                HTML('<h1 style="text-align:center;float:center;line-height:50px;">ShinyTropFish</h1>
<h3 style="text-align:center;float:center;line-height:50px;"> - This app allows you to conduct a length-based stock assessment based on ELEFAN, LCCC, and YPR - </h3>
<hr style="clear:both;"/>'),
div(img(src="TropFishROverviewPlot.png",width="60%"), style="text-align: center;"),
br(),
##tags$hr(),
br()

)
),

##-----------------------------------------------------------

## #087DEA


tabPanel("Load data", id = "loaddat",

         ##-----------------------------------------------------------
         headerPanel(title = "Load your data"),
         br(),

         tags$style(
                  HTML("hr{border-top: 2px solid #3891BA;}")),

         ## Sidebar panel for inputs
         sidebarPanel(div(style="display:inline-block;width:95%;text-align:center;",
                          ## Input: Select a file
                          fileInput("file1", "Choose your csv/txt file",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/x-csv",
                                               "text/tab-separated-values",
                                               "text/comma-separated-values",
                                               "text/x-comma-separated-values",
                                               "text/plain")),
                          actionButton("reset", label = " Reset",
                              style="color: #3891BA; background-color: #d35400; border-color: #d35400",
                              icon = icon("refresh", "fa-1.5x")
                              )
                          ),
                      br(),

                      "Please arrange your length measurements in an csv or txt file in the format explained in detail in this",a("tutorial.", href="https://cran.r-project.org/web/packages/TropFishR/vignettes/lfqData.html"),"Make sure that the headers of your data are called: 'length', 'dates', and 'frequency'.",

                      br(),
                      br(),

                      wellPanel(
                          fluidRow(
                              ## Input: Select separator
                              column(4, radioButtons("sep", "Separator",
                                                     choices = c(Comma = ",",
                                                                 Semicolon = ";",
                                                                 Tab = "\t",
                                                                 "White space"  = ""),
                                                     selected = ",")),
                              ## Input: Select quotes
                              column(4, radioButtons("quote", "Quote",
                                                     choices = c(None = "",
                                                                 "Double Quote" = '"',
                                                                 "Single Quote" = "'"),
                                                     selected = '"')),
                              ## Input: Select number of rows to display
                              column(4, radioButtons("disp", "Display",
                                                     choices = c(Head = "head",
                                                                 All = "all"),
                                                     selected = "head")))
                      ),
                      ## Input: Checkbox if file has header
                      checkboxInput("header", "Header", TRUE),

                      h3("Choose data type & assign columns"),
                      tags$hr(),
                      "Please choose the data type (raw length measurements vs. length-frequency table, see", a("(link),",target="_blank",href="lfqData.pdf"),"for more information). Further, please assign the columns of your data to the required columns. Press 'Update data' when all columns are assigned.",
                      br(),
                      br(),
                      checkboxInput(inputId = "isLFQ",
                                    label = "Length-frequency table",
                                    value = FALSE),
                      br(),
                      uiOutput("lengthCol"),
                      br(),
                      conditionalPanel(
                          condition = "input.isLFQ == 0",
                          ## Choose dates
                          uiOutput("dateCol"),
                          ## Choose frequceny
                          uiOutput("freqCol")
                      ),
                      br(),
                      conditionalPanel(
                          condition = "input.isLFQ == 1",
                          ## Choose range of freq cols
                          uiOutput("freqColsLFQ1"),
                          uiOutput("freqColsLFQ2"),
                      ),

                      br(),
                      textInput("dateFormat",
                                "Date format",
                                value = "%Y-%m-%d"),
                      br(),
                      checkboxInput(inputId = "aggDates",
                                    label = "Aggregate dates by month?",
                                    value = FALSE),
                      br(),

                      div(
                          style="display:inline-block;width:95%;text-align:center;",
                          actionButton(
                              "datUpdate",
                              label = " Update data",
                              style="color: #3891BA; background-color: #d35400; border-color: #d35400",
                              icon = icon("refresh", "fa-1.5x")
                          )
                      ),
                      br(),
                      br(),
                      br(),

                      h3("Example data"),
                      tags$hr(),
                      checkboxInput(inputId = "useExDat",
                                    label = "Use example data set?",
                                    value = FALSE),
                      br(),
                      conditionalPanel(
                          condition = "input.useExDat",
                          ## Input: Select example data set
                          selectInput(inputId = "exdat",
                                      "Example LFQ data sets",
                                      choices = c("alba",
                                                  "synLFQ4",
                                                  "synLFQ5",
                                                  "synLFQ6",
                                                  "synLFQ7",
                                                  "synLFQ8"),
                                      width='35%'),
                          ## Download example data
                          conditionalPanel("input.exdat",
                                           downloadLink('downloadExData', 'Download')))
                      ),


         ## Main panel for displaying outputs
         mainPanel(
             br(),
             h3("Uploaded file in raw format:"),
             tags$hr(),
             tableOutput("fileContentRaw"),
             br(),
             br(),
             h3("Data as length-frequency table"),
             tags$hr(),
             tableOutput("fileContent"),
             br()
         )
         ),
##-----------------------------------------------------------




tabPanel("Explore data", id = "explodat",
         ##-----------------------------------------------------------
         headerPanel(title = "Explore your data"),
         br(),
         sidebarLayout(
             sidebarPanel(id="sidebar",
                          br(),
                          ## tags$div(
                          ##          HTML("<font size='3'><b>LFQ restructuring</b></font>")),
                          h3("LFQ restructuring"),
                          tags$hr(),

                          ## choose number of bin size for restruct
                          uiOutput("binSize"),

                          ## sliderInput(inputId = "binSize",
                          ##                      label = "Bin size",
                          ##                      min = 0.5,
                          ##                      max = 8,
                          ##                      step = 0.5,
                          ##                      value = 1),

                          ## choose number of mas for restruct
                          sliderInput(inputId = "ma",
                                      label = "Moving average (MA) span",
                                      min = 3,
                                      max = 21,
                                      step = 2,
                                      value = 5,
                                      round = TRUE),

                          checkboxInput(inputId = "addlSqrt",
                                        label = "Apply additional squareroot transformation?",
                                        value = FALSE),

                          uiOutput("selYears"),


                          selectInput(inputId = "agg",
                                      label = "Aggregate dates by:",
                                      choices = c("Choose one"="",c("month","quarter","year")),
                                      selected = "month",
                                      width ='30%'),

                          uiOutput("plusGroup"),

                          br(),
                          br(),
                          br(),
                          ## tags$div(
                          ##          HTML("<font size='3'><b>Display options</b></font>")),
                          h3("Display options"),
                          tags$hr(),
                          checkboxInput(inputId = "relLFQ",
                                        label = "Display relative LFQ?",
                                        value = FALSE)

                          ## radioButtons(inputId = "catchVSrcounts",inline = TRUE,
                          ##              label = "Display catch or rcounts?",
                          ##              choiceValues = c("catch","rcounts"),
                          ##              choiceNames = c("catch","rcounts"),
                          ##              selected = "catch")

                          ),

             # Show a plot of the generated distribution
             mainPanel(

                 ### element styles
                 tags$style("#sampSize {font-size:20px;}"),
                 tags$style("#minL {font-size:20px;}"),
                 tags$style("#maxL {font-size:20px;}"),

                 #####
                 br(),

                 ## Overall sample size
                 textOutput(outputId = "sampSize"),

                 br(),

                 ## all sample years
                 textOutput(outputId = "sampYears"),

                 br(),

                 ## Min and max sizes in data
                 textOutput(outputId = "minL"),
                 textOutput(outputId = "maxL"),

                 br(),



                 tags$div(
                          HTML("<font size='4'>Overall LFQ plots:</font>")),

                 ## LFQ plot
                 plotOutput(outputId = "lfqPlot",
                            width = "70%"),

                 br(),

                 tags$div(
                          HTML("<font size='4'>Number of length measurements per month:</font>")),

                 br(),

                 ## Sample size per month per year
                 tableOutput(outputId = "sampPermonth"),

                 ## br(),

                 ## tags$div(
                 ##          HTML("<font size='4'>Detailed LFQ plots:</font>")),

                 ## ## LFQ plot
                 ## plotOutput(outputId = "lfqPlotDetailed"),

                 br()
             )
         )
         ),
##-----------------------------------------------------------




tabPanel("Growth", id = "growth",
         shinyjs::useShinyjs(),

         ##                 tags$style(HTML('#runELEFAN{background-color:orange; color:red;}')),

         ##-----------------------------------------------------------
         br(),
         headerPanel(title = "Estimate growth parameters"),
         br(),
         sidebarLayout(
             sidebarPanel(
                 "Find more information and a user manual about Electronic LEngth Frequency ANalysis (ELEFAN) in this",a("tutorial.", href="https://cran.r-project.org/web/packages/TropFishR/vignettes/Using_TropFishR_ELEFAN_functions.html")," Please be aware of the the best practices of this method listed in the above tutorial.",
                 br(),
                 br(),
                 br(),
                 div(style="display:inline-block;width:95%;text-align:center;",
                     actionButton("runELEFAN",
                                  label = "Run ELEFAN_GA",
                                  icon = icon("arrow-right")),
                     actionButton("resetELEFAN",
                                  label = "Clear")),
                 br(),
                 br(),
                 br(),
                 numericInput(inputId = "seed",
                              label = "Seed value",
                              min = 1,
                              max = Inf,
                              value = 1234,
                              width = '20%'),
                 br(),
                 br(),
                 ## tags$div(
                 ##          HTML("<font size='3'>LFQ restructuring</font>")),
                 h3("LFQ restructuring"),
                 tags$hr(),
                 br(),

                 ## choose number of bin size for restruct
                 ##                         uiOutput("binSizeGrowth"),

                 ##                 uiOutput("binSizeGrowth"),
                 sliderInput(inputId = "binSizeGrowth",
                             label = "Bin size",
                             min = 0.5,
                             max = 8,
                             step = 0.5,
                             value = 1),


                 sliderInput(inputId = "maGrowth",
                             label = "Moving average (MA) span",
                             min = 3,
                             max = 21,
                             step = 2,
                             value = 5,
                             round = TRUE),

                 br(),
                 br(),
                 br(),
                 br(),
                 ##                         tags$div(
                 ##                                  HTML("<font size='3'>Growth estimation settings</font>")),

                 h3("ELEFAN settings"),
                 tags$hr(),
                 tags$style("#seasonalized {font-size:15px; font-weight:bold;}"),
                 tags$style("
                         .checkbox label {
                             font-size: 18px;
                         }"),
                 checkboxInput(inputId = "seasonalized",
                               label = "Use seasonlized VBGF?",
                               value = FALSE),
                 br(),
                 uiOutput("Linfrange"),

                 sliderInput(
                     inputId = "Krange",
                     label = "K range",
                     dragRange = TRUE,
                     value = range(0.01, 1), min = 0.01, max = 3,
                     step = 0.01
                 ),

                 sliderInput(
                     inputId = "tarange",
                     label = "ta range",
                     dragRange = TRUE,
                     value = range(0, 1), min = 0, max = 1,
                     step = 0.1
                 ),

                 sliderInput(
                     inputId = "Crange",
                     label = "C range",
                     dragRange = TRUE,
                     value = c(0,1), min = 0, max = 1,
                     step = 0.1
                 ),

                 sliderInput(
                     inputId = "tsrange",
                     label = "ts range",
                     dragRange = TRUE,
                     value = range(0, 1), min = 0, max = 1,
                     step = 0.1
                 ),

                 sliderInput(
                     inputId = "popSize",
                     label = "Population size",
                     value = 50, min = 10, max = 500,
                     step = 5, round = TRUE
                 ),

                 sliderInput(
                     inputId = "pmutation",
                     label = "Prob. of mutation",
                     value = 0.2, min = 0, max = 1,
                     step = 0.1, round = TRUE
                 ),

                 sliderInput(
                     inputId = "maxiter",
                     label = "Maximum number of generations",
                     value = 20, min = 10, max = 190,
                     step = 5, round = TRUE
                 ),

                 sliderInput(
                     inputId = "run",
                     label = "Stopping criteria (generations without improvement)",
                     value = 20, min = 10, max = 190,
                     step = 5, round = TRUE
                 )),
             mainPanel(

                 ##                         tags$style("#pars {text-align: center;}"),
                 tags$h4("Growth"),
                 tags$hr(style = "border-top: dashed 2px #3891BA;"),
                 br(),
                 tags$h4("Fitted parameters:"),
                 tableOutput("pars"),
                 br(),
                 ## tags$div(style="display:inline-block;width:95%;text-align:center;",
                 ##          tableOutput("pars")
                 ##          ),
                 tags$h4("LFQ plot:"),
                 plotOutput("ELEFAN_GA_lfq_plot"),
                 br(),
                 tags$h4("GA fitness plot:"),
                 plotOutput("ELEFAN_GA_score_plot"),
                 br(),
                 tags$h4("Cohort plot:"),
                 plotOutput("ELEFAN_GA_cohort_plot"),
                 br(),
                 br(),
                 br(),
                 br(),
                 tags$h4("Recruitment"),
                 tags$hr(style = "border-top: dashed 2px #3891BA;"),
                 br(),
                 tags$h4("Estimates:"),
                 tableOutput("recruitPars"),
                 br(),
                 tags$h4("Plot:"),
                 plotOutput("recruit_plot")
             )

         )),
##-----------------------------------------------------------




tabPanel("Mortality/Selectivity", id = "mort",
         ##-----------------------------------------------------------
         br(),
         headerPanel(title = "Estimate mortality rates"),
         br(),
         sidebarLayout(
             sidebarPanel(id="sidebar",
                          "The Length-Converted Catch Curve (LCCC) is a length-based version of the catch curve for the estimation of the instantaneous total mortality rate (Z) from the slope of catches descending with increasing relative age. As t0 cannot be estimated with length-frequency data, the ages are relative.",
                          br(),
                          br(),
                          br(),
                          div(style="display:inline-block;width:95%;text-align: center;",
                              actionButton("runLCCC",
                                           label = "Run LCCC",
                                           icon = icon("arrow-right")),
                              actionButton("resetLCCC",
                                           label = "Clear")),
                          br(),
                          br(),
                          br(),
                          br(),
                          h3("LFQ restructuring"),
                          tags$hr(),
                          br(),

                          ## choose number of bin size for restruct
                          sliderInput(inputId = "binSizeMort",
                                      label = "Bin size",
                                      min = 0.5,
                                      max = 20,
                                      step = 0.5,
                                      value = 1),
                          br(),
                          br(),
                          br(),
                          h3("Length converted catch curve (LCCC) settings"),
                          tags$hr(),

                          ## choose points for regression line
                          uiOutput("regInt"),
                          br(),

                          ## choose catch column (for lfq data spanning multiple years)
                          uiOutput("selYearsLCCC"),
                          "If more than one year is chosen, the mortality estimates reflect the average over the entire time period.",

                          br(),
                          br(),
                          ## gotcha?
                          uiOutput("gotcha"),
                          br(),
                          ## choose points for regression line (gotcha)
                          uiOutput("regIntGotcha"),
                          br(),
                          ## use gotcha estimates?
                          checkboxInput(inputId = "gotchaEst",
                                        label = "Use GOTCHA estimates?",
                                        value = FALSE),
                          br(),
                          br(),
                          br(),
                          br(),
                          h3("Natural mortality"),
                          tags$hr(),
                          br(),

                          ## natural mortality method
                          selectInput(inputId = "natM",
                                      "Method:",
                                      choices = c("Then_growth",
                                                  "Pauly_Linf",
                                                  "Then_tmax"),
                                      selected = "Then_growth",
                                      width ='30%'),

                          ## schooling correction in Pauly's formula?
                          checkboxInput(inputId = "schooling",
                                        label = "Correction for schooling in fish?",
                                        value = FALSE),

                          ## slider for temperature
                          sliderInput(inputId = "temp",
                                      label = "Average ambient sea surface temperature:",
                                      min = 0,
                                      max = 40,
                                      step = 0.5,
                                      value = 20),


                          ## tmax for Then_tmax
                          numericInput(inputId = "tmax",
                                       label = "Maximum age",
                                       min = 0,
                                       max = 200,
                                       value = 10, width = '20%')
                          ),

             ## Show a plot of the generated distribution
             mainPanel(
                 tags$style("#mortPars {text-align: center;}"),
                 tags$h4("Fitted parameters:"),
                 br(),
                 tableOutput("mortPars"),
                 br(),
                 ## LCCC plot
                 tags$h4("LCCC plot:"),
                 plotOutput(outputId = "LCCC_plot",
                            width = "100%"),
                 br(),
                 ## LCCC selectivity plot
                 tags$h4("Selectivity plot:"),
                 plotOutput(outputId = "LCCC_sel_plot",
                            width = "100%"),
                 br(),
                 ## GOTCHA plot
                 tags$h4("GOTCHA parameters:"),
                 tableOutput("mortParsGotcha"),
                 br(),
                 tags$h4("GOTCH catch curve:"),
                 plotOutput(outputId = "LCCC_gotcha_plot",
                            width = "100%"),
                 br()
             )
         )
         ),
##-----------------------------------------------------------

tabPanel("Reference levels", id = "refs",
         ##-----------------------------------------------------------
         br(),
         headerPanel(title = "Estimate reference levels"),
         br(),
         sidebarLayout(
             sidebarPanel(id="sidebar",

                          br(),
                          div(style="display:inline-block;width:95%;text-align: center;",
                              actionButton("runYPR",
                                           label = "Run YPR",
                                           icon = icon("arrow-right")),
                              actionButton("resetYPR",
                                           label = "Clear")),
                          br(),
                          br(),
                          br(),
                          br(),
                          h3("LFQ restructuring"),
                          tags$hr(),
                          br(),

                          ## choose number of bin size for restruct
                          sliderInput(inputId = "binSizeYPR",
                                      label = "Bin size",
                                      min = 0.5,
                                      max = 20,
                                      step = 0.5,
                                      value = 1),
                          br(),
                          br(),
                          br(),
                          h3("Length-based YPR settings"),
                          tags$hr(),
                          textInput3(inputId="LWa", label="Constant ('a') of L-W relationship",
                                     value = 0.001, class="input-small"),
                          textInput3(inputId="LWb", label="Exponent ('b') of L-W relationship",
                                     value = 3, class="input-small"),
                          br(),
                          br(),

                          ## Lmat
                          uiOutput("Lmat"),
                          uiOutput("wmat"),

                          br(),
                          br(),

                          ## Lr
                          uiOutput("lr"),

                          ## selectivity from LCCC or knife edge?
                          radioButtons(inputId = "yprSel",inline = TRUE,
                                       label = "Knife edge or trawl-like selectivity?",
                                       choiceValues = c("TL","KE"),
                                       choiceNames = c("Trawl-like","Knife edge"),
                                       selected = "TL",
                                       width = '50%'
                                       ),

                          ## L50
                          uiOutput("l50"),

                          ## wqs
                          uiOutput("wqs"),

                          ## FM change vector
                          uiOutput("fmChangeAbs"),
                          uiOutput("fmChangeRel"),


                          ## length out of fm vector
                          numericInput(
                              inputId = "fmLengthOut",
                              label = "Number of steps for F vector",
                              value = 50,
                              min = 0,
                              width = '20%'
                          ),


                          ## Lc change vector
                          uiOutput("lcChange"),


                          ## length out of lc vector
                          numericInput(
                              inputId = "lcLengthOut",
                              label = "Number of steps for Lc vector",
                              value = 50,
                              min = 0,
                              width = '20%'
                          ),


                          ## stock size
                          numericInput(
                              inputId = "stockSize",
                              label = "Stock size",
                              value = 1,
                              min = 1,
                              width = '20%'
                          ),


                          ## Target SPR
                          sliderInput(
                              inputId = "targetSPR",
                              label = "Target SPR",
                              value = 0.75,
                              min = 0, max = 1,
                              step = 0.05
                          )

                          ),



             # Show a plot of the generated distribution
             mainPanel(
                 tags$h4("YPR"),
                 tags$hr(style = "border-top: dashed 2px #3891BA;"),
                 br(),
                 tags$h4("Fitted parameters:"),
                 tableOutput("yprPars"),
                 br(),
                 tags$h4("Yield per recruit plot:"),
                 plotOutput(outputId = "YPR_plot",
                            width = "100%"),
                 br(),
                 tags$h4("Yield per recruit plot (F vs Lc):"),
                 plotOutput(outputId = "YPR_Lc_plot",
                            width = "100%"),
                 br(),
                 tags$h4("Biomass per recruit plot (F vs Lc):"),
                 plotOutput(outputId = "YPR_Lc2_plot",
                            width = "100%"),
                 br(),
                 br()
             )
         )
         ),
##-----------------------------------------------------------



tabPanel("Overview", id = "overview",
         ##-----------------------------------------------------------
         br(),
         headerPanel(title = "All results"),
         br(),
         sidebarLayout(
             ##               left = "20%", right = "auto", bottom = "auto",
             ##               width = "80%", height = "auto",
             sidebarPanel(id="sidebar",
                          br(),
                          "Generate an Rmarkdown-based assessment report summarising the results of the assessment of your data:",
                          br(),
                          br(),
                          actionButton("generateReport", "Generate Assessment Report",
                                       icon = icon("file")),
                          br(),
                          br(),
                          "Download the assessment report:",
                          br(),
                          br(),
                          conditionalPanel(condition = "output.reportbuilt",
                                           downloadButton("downloadReport",
                                                          "Download Assessment Report")),
                          br(),
                          br(),
                          br(),
                          br(),
                          'Download all estimated parameters as a "csv" file:',
                          br(),
                          br(),
                          downloadButton("allParameters",
                                         label = "Download all parameters"),
                          br(),
                          br(),
                          br(),
                          br(),
                          'Download all graphs as a pdf files in a "zip" archive:',
                          br(),
                          br(),
                          downloadButton("allGraphs",
                                         label = "Download all graphs"),
                          br(),
                          br(),
                          br(),
                          br(),
                          'Download all data as a "RData" file:',
                          br(),
                          br(),
                          downloadButton("allData",
                                         label = "Download all data"),
                          br(),
                          br(),
                          br()
                          ),
             mainPanel(
                 br(),
                 tags$h4("All parameters"),
                 tags$hr(),
                 br(),

                 tableOutput("allpars_ov"),
                 br(),
                 br(),
                 br(),


                 tags$h4("Growth"),
                 tags$hr(),
                 br(),

                 tableOutput("growthPars_ov"),
                 br(),
                 tags$h4("LFQ plot:"),
                 plotOutput("ELEFAN_GA_lfq_plot_ov"),
                 br(),
                 tags$h4("GA fitness plot:"),
                 plotOutput("ELEFAN_GA_score_plot_ov"),
                 br(),
                 tags$h4("Cohort plot:"),
                 plotOutput("ELEFAN_GA_cohort_plot_ov"),

                 br(),
                 br(),
                 br(),

                 tags$h4("Recruitment"),
                 tags$hr(),
                 br(),
                 tableOutput("recruitPars_ov"),
                 br(),
                 plotOutput(outputId = "recruit_plot_ov",
                            width = "100%"),
                 br(),
                 br(),
                 br(),


                 tags$h4("Mortality rates"),
                 tags$hr(),
                 br(),
                 tableOutput("mortPars_ov"),
                 br(),
                 plotOutput(outputId = "LCCC_plot_ov",
                            width = "100%"),
                 br(),
                 plotOutput(outputId = "LCCC_sel_plot_ov",
                            width = "100%"),
                 br(),
                 "Gotcha parameters:",
                 tableOutput("mortParsGotcha_ov"),
                 br(),
                 plotOutput(outputId = "LCCC_gotcha_plot_ov",
                            width = "100%"),
                 br(),
                 br(),
                 br(),

                 tags$h4("Reference levels"),
                 tags$hr(),
                 br(),

                 tags$h4("YPR"),
                 tags$hr(style = "border-top: dashed 2px #3891BA;"),
                 br(),
                 tableOutput("yprPars_ov"),
                 br(),
                 plotOutput(outputId = "YPR_plot_ov",
                            width = "100%"),
                 br(),
                 "Yield per recruit",
                 plotOutput(outputId = "YPR_Lc_plot_ov",
                            width = "100%"),
                 br(),
                 "Biomass per recruit",
                 plotOutput(outputId = "YPR_Lc2_plot_ov",
                            width = "100%"),
                 br(),
                 br()
             )
         )
         ),
##-----------------------------------------------------------

##-----------------------------------------------------------
tabPanel("References", id = "references",
         br(),
         h3("Scientific articles:"),
         tags$hr(),
         "Mildenberger TK, Taylor MH, Kokkalis A, Pauly D. 2019, Fish stock assessment with length-frquency data: A novel app and best practices. Fisheries Research. doi:...",a("COMING",href=""),
         br(),
         br(),
         "Mildenberger TK, Taylor MH, Wolff M. 2017, TropFishR: an R package for fisheries analysis with length‐frequency data. Methods Ecol Evol, 8: 1520-1527.",a("doi:10.1111/2041-210X.12791",href="https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12791"),
         br(),
         br(),
         "Taylor MH, Mildenberger TK. 2017, Extending electronic length frequency analysis in R. Fish Manag Ecol. 2017;24:330–338.",a("https://doi.org/10.1111/fme.12232",href="https://onlinelibrary.wiley.com/doi/full/10.1111/fme.12232"),
         tags$hr(),
         br(),
         br(),
         br(),
         h3("Tutorials:"),
         tags$hr(),
         "Mildenberger, TK. 2018. Length-frequency data for TropFishR. ",a("Link",href="https://cran.r-project.org/web/packages/TropFishR/vignettes/lfqData.html"),
         br(),
         br(),
         "Taylor, MH. 2018. Using the TropFishR ELEFAN functions. ",a("Link",href="https://cran.r-project.org/web/packages/TropFishR/vignettes/Using_TropFishR_ELEFAN_functions.html"),
         br(),
         br(),
         "Mildenberger, TK. 2017. Single-species fish stock assessment with TropFishR. ",a("Link",href="https://cran.r-project.org/web/packages/TropFishR/vignettes/tutorial.html"),
         tags$hr(),
         br(),
         br(),
         br(),
         br()
         ),
tabPanel("About", id = "about",
         br(),
         tags$h2("ShinyTropFish"),
         tags$hr(),
         'Shiny app for Tropical Fisheries Analysis in R. The click-based user interface for the R package TropFishR. TropFishR is a collection of fisheries models based on the FAO Manual "Introduction to tropical fish stock assessment" by Sparre and Venema (1998, 1999). Not only scientists working in the tropics will benefit from this new toolbox. The methods work with age-based or length-frequency data and assist in the assessment of data poor fish stocks. Overall, the package comes with 30 functions, 19 data sets and 10 s3 methods. All objects are documented and provide examples that allow reproducing the examples from the FAO manual.',
         br(),
         br(),
         br(),
         br(),
         tags$h2("Version"),
         tags$hr(),
         "Version number: v0.1 (Beta)",
         br(),
         br(),
         br(),
         br(),
         tags$h2("News"),
         tags$hr(),
         "This is the beta version of ShinyTropFish. You can find detailed descriptions of new features, bug fixes, other changes of specific package versions concerning", a("ShinyTropFish",href="https://rawgit.com/tokami/TropFishR/master/inst/doc/news.html"), " and concerning", a("TropFishR",href="https://rawgit.com/tokami/TropFishR/master/inst/doc/news.html"),
         br(),
         br(),
         br(),
         br(),
         tags$h2("Installation for offline use"),
         tags$hr(),
         'This application can be used offline. Please download the package from', a("GitHub",href="https://github.com/tokami/ShinyTropFish"), 'with devtools::install_github("tokami/ShinyTropFish") and run the R commands: require(ShinyTropFish) and runApp("apps/").',
         br(),
         br(),
         br(),
         br(),
         tags$h2("Citation"),
         tags$hr(),
         "Please cite this application as:",
         "Mildenberger TK, Taylor MH, Kokkalis A, Pauly D. 2019, Fish stock assessment with length-frquency data: A novel app and best practices. Fisheries Research. doi:...",a("COMING",href=""),
         br(),
         br(),
         br(),
         br(),
         tags$h2("Questions/Issues"),
         tags$hr(),
         "In case you have questions or find bugs, please write an email to",
         a("Tobias Mildenberger",href="mailto:t.k.mildenberger@gmail.com"), "or post on",
         a("ShinyTropFish/issues",href="https://github.com/tokami/ShinyTropFish/issues"), ". If you want to be updated with the development of the application and underlying R package (TroFishR) or want to discuss with ShinyTropFish and TropFishR users and developers, follow the project on", a("ResearchGate", href="https://www.researchgate.net/project/TropFishR"),".",
         br(),
         br(),
         br(),
         br(),
         tags$h2("Developer team"),
         tags$hr(),
         box(title = "Tobias K. Mildenberger",
             status = "primary",
             solidHeader = TRUE,
             collapsible = FALSE,
             background="black",
             width = 3,
             height = 1,
             fluidRow(column(width = 4, align = "center", imageOutput("tobm")),
                      column(width = 8, align = "left", h5("Creator, Author, Maintainer"),  HTML("DTU AQUA<br/>National Institute of Aquatic Resources<br/>Technical University of Denmark<br/>Kemitorvet<br/>2800 Kgs. Lyngby<br/>Denmark"))
                      )),
         box(title = "Marc H. Taylor",
             status = "primary",
             solidHeader = TRUE,
             collapsible = FALSE,
             width = 3,
             height = 1,
             fluidRow(column(width = 4, align = "center", imageOutput("mht")),
                      column(width = 8, align = "left", h5("Author"), HTML("Thuenen Institute of Sea Fisheries<br/>Herwigstrasse 31<br/>27572 Bremerhaven<br/>Germany"))
                      )),
         box(title = "Alexandros Kokkalis",
             status = "primary",
             solidHeader = TRUE,
             collapsible = FALSE,
             width = 3,
             height = 1,
             fluidRow(column(width = 4, align = "center", imageOutput("alko")),
                      column(width = 8, align = "left", h5("Author"), HTML("DTU AQUA<br/>National Institute of Aquatic Resources<br/>Technical University of Denmark<br/>Kemitorvet<br/>2800 Kgs. Lyngby<br/>Denmark"))
                      )),
         box(title = "Daniel Pauly",
             status = "primary",
             solidHeader = TRUE,
             collapsible = FALSE,
             width = 3,
             height = 1,
             fluidRow(column(width = 4, align = "center", imageOutput("dp")),
                      column(width = 8, align = "left", h5("Author"), HTML("Institute for the Oceans and Fisheries<br/>The University of British Columbia<br/>2202 Main Mall<br/>Vancouver, British Columbia<br/>Canada"))
                      )),
         br()

         ),

tabPanel(
    title = "Quit ",
    value = "stop",
    icon = icon("power-off")
),


tabPanel(
    textOutput("filename")
)
)
)
##)
)
##-----------------------------------------------------------
