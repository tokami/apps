## GUI for spictapp
## Tobias K. Mildenberger
## January 2020

## Load packages
##-----------------------------------------------------------------------------------
library(shiny)
## options(shiny.trace=TRUE)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
## library(shinyWidgets)
## library(markdown)
library(rmarkdown)
## library(googleAuthR)
library(htmltools)
library(spict)


## Load scripts
##-----------------------------------------------------------------------------------
source("../funcs/uiFuncs.R")

## UI
##-----------------------------------------------------------------------------------
shinyUI(
    fluidPage(

        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(text = jscode, functions=c("closeWindow")),

        ## style settings
        tags$head(
                 tags$style(
                          HTML("
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(11) {
                           float: right;
                           font-weight: bold;
                           color: black;
                           }

                           ")
                      )
             ),
        tags$style(
                 type="text/css",
                 "textarea {width:100%}"
             ),

        navbarPage(
            title = "spictapp",
            id = "tabset",
            theme = shinytheme("united"),
            inverse = FALSE,
            collapsible = TRUE,

            tabPanel(
                "Home", id = "home",
                tagList(
                    HTML('<h1 style="text-align:center;float:center;line-height:50px;">spictapp</h1><h3 style="text-align:center;float:center;line-height:50px;"> - The Shiny app for the Stochastic surplus Production model in Continuous Time (SPiCT) - </h3><hr style="clear:both;"/>'), br(),
                    div(img(src="spictManageDemo.png"), style="text-align: center;"),
                    br(),
                    tags$hr(),
                    br(),
                    br(),
                    br()
                )
            ),

            tabPanel(
                "Load data", id = "loaddat",
                ##-----------------------------------------------------------
                headerPanel(title = "Load input data"),
                br(),

                tags$style(
                         HTML("hr{border-top: 2px solid #d35400;}")
                     ),

                tags$style("
             .btn-file {
             background-color:#d35400;
             border-color: #d35400;
             }

             .progress-bar {
             background-color: #d35400;
             }

             "),

             ## Sidebar panel for inputs
             sidebarPanel(div(
                 style="display:inline-block;width:95%;text-align:center;align:center;",
                 h3("Upload data file"),
                 tags$hr(),
                 ## Input: Select a file
                 fileInput("file1", "Choose a csv/txt file",
                           multiple = FALSE,
                           ##                                  style="color: #fff; background-color: #d35400; border-color: #d35400",
                           accept = c("text/csv",
                                      "text/x-csv",
                                      "text/tab-separated-values",
                                      "text/comma-separated-values",
                                      "text/x-comma-separated-values",
                                      "text/plain")
                           ),
                 "Please use reset before uploading a new data set:",
                 br(),
                 br(),
                 actionButton("reset", label = " Reset",
                              style="color: #fff; background-color: #d35400; border-color: #d35400",
                              icon = icon("refresh", "fa-1.5x")
                              )
             ), br(), br(),
             "Your file must contain at least 3 columns: One vector with the times corresponding to the observations, one with the commercial catch observations, and one with either index or effort observations. The app tries to intrepret the column names of your data automatically, but might not be successfull in assigning all columns. If the 'Data with assigned columns' is empty or did not assign the columns correctly, please refer to the 'Assign columns' section below and press 'Update data' when done.",
             br(), br(),
             h3("File properties"),
             tags$hr(),
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
                                            selected = "head")
                            )
                 ),
                 ## Input: Checkbox if file has header
                 checkboxInput("header", "Header", TRUE)
             ),


             h3("Assign columns"),
             tags$hr(),
             "Please assign the columns of your data to the required SPiCT input data. SPiCT requires a vector with catch observations and their times, as well as either index observations and their times or effort observations and their times. Press 'Update data' when all columns are assigned.",
             br(),
             br(),
             "Commercial catch:",

             wellPanel(
                 fluidRow(
                     column(6,
                            ## Time catches
                            uiOutput("timeC_lab")
                            ),
                     column(6,
                            ## Catch observations
                            uiOutput("obsC_lab")
                            )
                 )
             ),
             "Indices from scientific surveys:",
             wellPanel(
                 fluidRow(
                     column(6,
                            uiOutput("timeI_lab")
                            ),
                     column(6,
                            uiOutput("obsI_lab")
                            )
                 ),
                 "It is possible to select multiple columns representing different index observations (e.g. differen survey fleets) and their times."
             ),
             "Effort information (optional):",
             wellPanel(
                 fluidRow(
                     column(6,
                            ## Time effort
                            uiOutput("timeE_lab")
                            ),
                     column(6,
                            ## Effort observations
                            uiOutput("obsE_lab")
                            )
                 ),
                 "Effort observations are optional if indices are available and required otherwise."
             ),

             div(
                 style="display:inline-block;width:95%;text-align:center;",
                 actionButton(
                     "datUpdate",
                     label = " Update data",
                     style="color: #fff; background-color: #d35400; border-color: #d35400",
                     icon = icon("refresh", "fa-1.5x")
                 )
             ),
             br(),
             br(),
             br(),

             "If information about the uncertainty of the observations is available, it can be provided as a",
             tags$b("factor scaling the uncertainty of the observations."), "This variables is called", tags$b("stdevfac"),
             "for the different observations in SPiCT, e.g. stdevfacC for catches.",
             "Several columns can be selected if several indices are available.",

             wellPanel(
                 fluidRow(
                     uiOutput("stdevfacC_lab"),
                     uiOutput("stdevfacI_lab"),
                     uiOutput("stdevfacE_lab")
                 )
             ),
             br(),
             h3("Use example data"),
             tags$hr(),
             checkboxInput(inputId = "useExDat",
                           label = "Use example data set?",
                           value = FALSE),
             br(),
             conditionalPanel(
                 condition = "input.useExDat",
                 ## Input: Select example data set
                 selectInput(inputId = "exdat",
                             "Example data sets",
                             choices = c("albacore",
                                         "hake",
                                         "lobster"),
                             width='35%'),
                 ## Download example data
                 conditionalPanel("input.exdat",
                                  downloadLink('downloadExData', 'Download')
                                  )
             )
             ),


             ## Main panel for displaying outputs
             mainPanel(
                 br(),
                 h3("Uploaded file in raw format:"),
                 tags$hr(),
                 tableOutput("fileContentRaw"),
                 br(),
                 br(),
                 h3("Data with assigned columns:"),
                 tags$hr(),
                 tableOutput("fileContent"),
                 br()
                 ## fluidRow(
                 ##     column(
                 ##         6,
                 ##         h3("Uploaded file in raw format:"),
                 ##         tags$hr()
                 ##     ),
                 ##     column(
                 ##         6,
                 ##         h3("Data with assigned columns:"),
                 ##         tags$hr()
                 ##     ),
                 ##     br(),
                 ##     br(),
                 ##     br(),
                 ##     column(
                 ##         6,
                 ##         dataTableOutput("fileContentRaw")
                 ##     ),
                 ##     column(
                 ##         6,
                 ##         dataTableOutput("fileContent")
                 ##     )
                 ## )
             )
            ),

            tabPanel(
                "Explore & modify data", id = "explodat",
                ##-----------------------------------------------------------
                headerPanel(title = "Explore & modify input data"),
                br(),

                tags$style(
                         HTML("hr{border-top: 2px solid #d35400;}")
                     ),

                sidebarLayout(
                    sidebarPanel(
                        id="sidebar",
                        br(),
                        h3("General settings"),
                        tags$hr(),
                        fluidRow(
                            ## choose dteuler
                            column(6,
                                   selectInput(inputId = "dteuler",
                                               label = "dteuler",
                                               choices = c("1/64"=1/64,"1/32"=1/32,
                                                           "1/16"=1/16,"1/8"=1/8,"1/4"=1/4,
                                                           "1/2"=1/2,"1"=1),
                                               selected = 1/16,
                                               width = '100%'
                                               )
                                   ),
                            column(
                                6,
                                "Euler discretisation time step, i.e. how many time steps per year should be used."
                            )
                        ),
                        br(),
                        fluidRow(
                            column(6,
                                   ## Index timings
                                   selectizeInput("timeIshift",
                                                  "Index timing",
                                                  choices = NULL,
                                                  multiple = TRUE,
                                                  options = list(create = TRUE),
                                                  width = "100%"
                                                  ),
                                   "Adjust the timing of the index within the year (e.g. 0.25 for April)"
                                   ),
                            ## Catch unit
                            column(6,
                                   textInput(
                                       "cunit",
                                       "Catch unit",
                                       "",
                                       width = "100%"),
                                   "Set the unit of the catches (to be displayed in the graphs), e.g. '000 t."
                                   )
                        ),
                        br(),
                        br(),
                        ## choose time range of observations
                        uiOutput("timerange"),
                        br(),
                        checkboxInput(
                            "robflagc",
                            "Should the robust estimation for catches be used?",
                            value = FALSE
                        ),
                        br(),
                        uiOutput("robflagi"),
                        checkboxInput(
                            "robflage",
                            "Should the robust estimation for effort be used?",
                            value = FALSE
                        ),
                        br(),
                        "Settings concerning the process used to model the fishing mortalit process. With nseasons = 1 and seasontype = 0, the annual F process is used. Seasontype = 1 uses splines to fit the seasonal F model, seasontype = 2 uses coupled SDEs, and seaosntype = 3 uses splines together with a auto-correlated process. The splineorder is only used if a spline-based process is used.",
                        br(),
                        br(),
                        br(),
                        fluidRow(
                            ## Number of seasons
                            column(
                                4,
                                uiOutput("nseasons"),
                                ""
                            ),
                            ## seasontype
                            column(
                                4,
                                uiOutput("seasontype")
                            ),
                            ## Spline order
                            column(
                                4,
                                selectInput("splineorder",
                                            "Splineorder",
                                            c(2,3),
                                            selected = 2,
                                            width = "100%")
                            )
                        ),
                        br(),
                        br(),
                        h3("Management settings"),
                        tags$hr(),
                        "By default, SPiCT projects the model states one year into the future starting after the last observation. Any forecast/management interval can be defined. The F factor multiplies the fishing mortality at the beginning of the management interval.",
                        br(),
                        br(),
                        br(),
                        ## management interval
                        div(
                            style="display:inline-block;width:70%;text-align:center;font-weight:bold;",
                            column(4,),
                            uiOutput("maninterval")
                        ),
                        br(),
                        ## management evaluation time
                        uiOutput("maneval"),
                        br(),
                        fluidRow(
                            column(
                                6,
                                numericInput(
                                    "ffac",
                                    "F multiplication factor",
                                    value = 1,
                                    min = 0,
                                    width = "100%"
                                )),
                            column(
                                6,
                                numericInput(
                                    "fcon",
                                    "Absolute F",
                                    value = 0,
                                    min = 0,
                                    width = "100%"
                                ))),
                        br(),
                        h3("Priors"),
                        tags$hr(),
                        "Default priors",
                        wellPanel(
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "lognPrior",
                                        "log(n)",
                                        TRUE)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "lognMu",
                                        "mu",
                                        2)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "lognSd",
                                        "sd",
                                        2)
                                ),
                                column(
                                    2,
                                    br(),
                                    checkboxInput(
                                        "lognLog",
                                        "log(mu)",
                                        TRUE)
                                )
                            ),
                            ## logalpha
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "logAlphaPrior",
                                        "log(alpha)",
                                        TRUE)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logAlphaMu",
                                        "mu",
                                        1)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logAlphaSd",
                                        "sd",
                                        2)
                                ),
                                column(
                                    2,
                                    br(),
                                    checkboxInput(
                                        "logAlphaLog",
                                        "log(mu)",
                                        TRUE)
                                )
                            ),
                            ##logbeta
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "logBetaPrior",
                                        "log(beta)",
                                        TRUE)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logBetaMu",
                                        "mu",
                                        1)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logBetaSd",
                                        "sd",
                                        2)
                                ),
                                column(
                                    2,
                                    br(),
                                    checkboxInput(
                                        "logBetaLog",
                                        "log(mu)",
                                        TRUE)
                                )
                            )
                        ),
                        br(),
                        "Additional priors",
                        wellPanel(
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "BmsyB0Prior",
                                        "Bmsy/B0",
                                        FALSE),
                                    ),
                                column(
                                    3,
                                    numericInput(
                                        "BmsyB0Mu",
                                        "mu",
                                        1)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "BmsyB0Sd",
                                        "sd",
                                        2)
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "logbkfracPrior",
                                        "log(B/K)",
                                        FALSE),
                                    ),
                                column(
                                    3,
                                    numericInput(
                                        "logbkfracMu",
                                        "mu",
                                        1)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logbkfracSd",
                                        "sd",
                                        2)
                                ),
                                column(
                                    2,
                                    br(),
                                    checkboxInput(
                                        "logbkfracLog",
                                        "log(mu)",
                                        FALSE)
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "logngammaPrior",
                                        "log(ngamma)",
                                        FALSE),
                                    ),
                                column(
                                    3,
                                    numericInput(
                                        "logngammaMu",
                                        "mu",
                                        1)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logngammaSd",
                                        "sd",
                                        2)
                                ),
                                column(
                                    2,
                                    br(),
                                    checkboxInput(
                                        "logngammaLog",
                                        "log(mu)",
                                        FALSE)
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "logKPrior",
                                        "log(K)",
                                        FALSE),
                                    ),
                                column(
                                    3,
                                    numericInput(
                                        "logKMu",
                                        "mu",
                                        1)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logKSd",
                                        "sd",
                                        2)
                                ),
                                column(
                                    2,
                                    br(),
                                    checkboxInput(
                                        "logKLog",
                                        "log(mu)",
                                        FALSE)
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "logrPrior",
                                        "log(r)",
                                        FALSE),
                                    ),
                                column(
                                    3,
                                    numericInput(
                                        "logrMu",
                                        "mu",
                                        1)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logrSd",
                                        "sd",
                                        2)
                                ),
                                column(
                                    2,
                                    br(),
                                    checkboxInput(
                                        "logrLog",
                                        "log(mu)",
                                        FALSE)
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "logmPrior",
                                        "log(m)",
                                        FALSE),
                                    ),
                                column(
                                    3,
                                    numericInput(
                                        "logmMu",
                                        "mu",
                                        1)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logmSd",
                                        "sd",
                                        2)
                                ),
                                column(
                                    2,
                                    br(),
                                    checkboxInput(
                                        "logmLog",
                                        "log(mu)",
                                        FALSE)
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "logqPrior",
                                        "log(q)",
                                        FALSE),
                                    ),
                                column(
                                    3,
                                    numericInput(
                                        "logqMu",
                                        "mu",
                                        1)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logqSd",
                                        "sd",
                                        2)
                                ),
                                column(
                                    2,
                                    br(),
                                    checkboxInput(
                                        "logqLog",
                                        "log(mu)",
                                        FALSE)
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    conditionalPanel("output.noSurv >= 2",
                                                     checkboxInput(
                                                         "logqPrior2",
                                                         "log(q2)",
                                                         FALSE)
                                                     )
                                ),
                                column(
                                    3,
                                    conditionalPanel("output.noSurv >= 2",
                                                     numericInput(
                                                         "logqMu2",
                                                         "mu",
                                                         1)
                                                     )
                                ),
                                column(
                                    3,
                                    conditionalPanel("output.noSurv >= 2",
                                                     numericInput(
                                                         "logqSd2",
                                                         "sd",
                                                         2)
                                                     )
                                ),
                                column(
                                    2,
                                    br(),
                                    conditionalPanel("output.noSurv >= 2",
                                                     checkboxInput(
                                                         "logqLog2",
                                                         "log(mu)",
                                                         FALSE)
                                                     )
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    conditionalPanel("output.noSurv >= 3",
                                                     checkboxInput(
                                                         "logqPrior3",
                                                         "log(q3)",
                                                         FALSE)
                                                     )
                                ),
                                column(
                                    3,
                                    conditionalPanel("output.noSurv >= 3",
                                                     numericInput(
                                                         "logqMu3",
                                                         "mu",
                                                         1)
                                                     )
                                ),
                                column(
                                    3,
                                    conditionalPanel("output.noSurv >= 3",
                                                     numericInput(
                                                         "logqSd3",
                                                         "sd",
                                                         2)
                                                     )
                                ),
                                column(
                                    2,
                                    br(),
                                    conditionalPanel("output.noSurv >= 3",
                                                     checkboxInput(
                                                         "logqLog3",
                                                         "log(mu)",
                                                         FALSE)
                                                     )
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    conditionalPanel("output.noSurv >= 4",
                                                     checkboxInput(
                                                         "logqPrior4",
                                                         "log(q4)",
                                                         FALSE)
                                                     )
                                ),
                                column(
                                    3,
                                    conditionalPanel("output.noSurv >= 4",
                                                     numericInput(
                                                         "logqMu4",
                                                         "mu",
                                                         1)
                                                     )
                                ),
                                column(
                                    3,
                                    conditionalPanel("output.noSurv >= 4",
                                                     numericInput(
                                                         "logqSd4",
                                                         "sd",
                                                         2)
                                                     )
                                ),
                                column(
                                    2,
                                    br(),
                                    conditionalPanel("output.noSurv >= 4",
                                                     checkboxInput(
                                                         "logqLog4",
                                                         "log(mu)",
                                                         FALSE)
                                                     )
                                )
                            ),

                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "logqfPrior",
                                        "log(qf)",
                                        FALSE),
                                    ),
                                column(
                                    3,
                                    numericInput(
                                        "logqfMu",
                                        "mu",
                                        1)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logqfSd",
                                        "sd",
                                        2)
                                ),
                                column(
                                    2,
                                    br(),
                                    checkboxInput(
                                        "logqfLog",
                                        "log(mu)",
                                        FALSE)
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "logsdbPrior",
                                        "log(sdb)",
                                        FALSE),
                                    ),
                                column(
                                    3,
                                    numericInput(
                                        "logsdbMu",
                                        "mu",
                                        1)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logsdbSd",
                                        "sd",
                                        2)
                                ),
                                column(
                                    2,
                                    br(),
                                    checkboxInput(
                                        "logsdbLog",
                                        "log(mu)",
                                        FALSE)
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "logsdiPrior",
                                        "log(sdi)",
                                        FALSE),
                                    ),
                                column(
                                    3,
                                    numericInput(
                                        "logsdiMu",
                                        "mu",
                                        1)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logsdiSd",
                                        "sd",
                                        2)
                                ),
                                column(
                                    2,
                                    br(),
                                    checkboxInput(
                                        "logsdiLog",
                                        "log(mu)",
                                        FALSE)
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    conditionalPanel("output.noSurv >= 2",
                                                     checkboxInput(
                                                         "logsdiPrior2",
                                                         "log(sdi2)",
                                                         FALSE)
                                                     )
                                ),
                                column(
                                    3,
                                    conditionalPanel("output.noSurv >= 2",
                                                     numericInput(
                                                         "logsdiMu2",
                                                         "mu",
                                                         1)
                                                     )
                                ),
                                column(
                                    3,
                                    conditionalPanel("output.noSurv >= 2",
                                                     numericInput(
                                                         "logsdiSd2",
                                                         "sd",
                                                         2)
                                                     )
                                ),
                                column(
                                    2,
                                    br(),
                                    conditionalPanel("output.noSurv >= 2",
                                                     checkboxInput(
                                                         "logsdiLog2",
                                                         "log(mu)",
                                                         FALSE)
                                                     )
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    conditionalPanel("output.noSurv >= 3",
                                                     checkboxInput(
                                                         "logsdiPrior3",
                                                         "log(sdi3)",
                                                         FALSE)
                                                     )
                                ),
                                column(
                                    3,
                                    conditionalPanel("output.noSurv >= 3",
                                                     numericInput(
                                                         "logsdiMu3",
                                                         "mu",
                                                         1)
                                                     )
                                ),
                                column(
                                    3,
                                    conditionalPanel("output.noSurv >= 3",
                                                     numericInput(
                                                         "logsdiSd3",
                                                         "sd",
                                                         2)
                                                     )
                                ),
                                column(
                                    2,
                                    br(),
                                    conditionalPanel("output.noSurv >= 3",
                                                     checkboxInput(
                                                         "logsdiLog3",
                                                         "log(mu)",
                                                         FALSE)
                                                     )
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    conditionalPanel("output.noSurv >= 4",
                                                     checkboxInput(
                                                         "logsdiPrior4",
                                                         "log(sdi4)",
                                                         FALSE)
                                                     )
                                ),
                                column(
                                    3,
                                    conditionalPanel("output.noSurv >= 4",
                                                     numericInput(
                                                         "logsdiMu4",
                                                         "mu",
                                                         1)
                                                     )
                                ),
                                column(
                                    3,
                                    conditionalPanel("output.noSurv >= 4",
                                                     numericInput(
                                                         "logsdiSd4",
                                                         "sd",
                                                         2)
                                                     )
                                ),
                                column(
                                    2,
                                    br(),
                                    conditionalPanel("output.noSurv >= 4",
                                                     checkboxInput(
                                                         "logsdiLog4",
                                                         "log(mu)",
                                                         FALSE)
                                                     )
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "logsdfPrior",
                                        "log(sdf)",
                                        FALSE),
                                    ),
                                column(
                                    3,
                                    numericInput(
                                        "logsdfMu",
                                        "mu",
                                        1)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logsdfSd",
                                        "sd",
                                        2)
                                ),
                                column(
                                    2,
                                    br(),
                                    checkboxInput(
                                        "logsdfLog",
                                        "log(mu)",
                                        FALSE)
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "logsdePrior",
                                        "log(sde)",
                                        FALSE),
                                    ),
                                column(
                                    3,
                                    numericInput(
                                        "logsdeMu",
                                        "mu",
                                        1)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logsdeSd",
                                        "sd",
                                        2)
                                ),
                                column(
                                    2,
                                    br(),
                                    checkboxInput(
                                        "logsdeLog",
                                        "log(mu)",
                                        FALSE)
                                )
                            ),
                            fluidRow(
                                column(
                                    3,
                                    br(),
                                    checkboxInput(
                                        "logsdcPrior",
                                        "log(sdc)",
                                        FALSE),
                                    ),
                                column(
                                    3,
                                    numericInput(
                                        "logsdcMu",
                                        "mu",
                                        1)
                                ),
                                column(
                                    3,
                                    numericInput(
                                        "logsdcSd",
                                        "sd",
                                        2)
                                ),
                                column(
                                    2,
                                    br(),
                                    checkboxInput(
                                        "logsdcLog",
                                        "log(mu)",
                                        FALSE)
                                )
                            )
                        ),
                        br(),
                        br(),
                        h3("Display options"),
                        tags$hr(),
                        checkboxInput(inputId = "dataplotAdv",
                                      label = "Plot the advanced data plot?",
                                      value = FALSE),
                        ),

                    # Show a plot of the generated distribution
                    mainPanel(
                        h3("SPiCT timeline"),
                        tags$hr(),
                        div(style="display:inline-block;width:100%;text-align:left;",
                            verbatimTextOutput(outputId = "mantimeline")),
                        br(),
                        br(),
                        h3("Plot of input data"),
                        tags$hr(),
                        plotOutput(outputId = "dataplot",
                                   width = "100%", height = "800px"),
                        br(),
                        br(),
                        h3("Input data"),
                        tags$hr(),
                        verbatimTextOutput(outputId = "inpsum"),
                        br(),
                        br(),
                        h3("Priors"),
                        tags$hr(),
                        plotOutput(outputId = "priorplot",
                                   width = "100%", height = "500px"),
                        br()
                    )
                )
            ),

            tabPanel(
                "Fit SPiCT", id = "fitspict",
                shinyjs::useShinyjs(),
                br(),
                headerPanel(title = "Fit SPiCT"),

                tags$style(
                         HTML("hr{border-top: 2px solid #d35400;}")),

                br(),
                sidebarLayout(
                    sidebarPanel(
                        "For more information about SPiCT, please refer to the SPiCT handbook",
                        a("(link),",target="_blank",href="spict_handbook.pdf"), "the SPiCT guidelines",
                       a("(link)",target="_blank",href="spict_guidelines.pdf"),
                        "or the peer-reviewed SPiCT publications:",
                        a("(link)",target="_blank",href="spict.pdf"), "and",
                       a("(link).",target="_blank",href="spict_tvp.pdf"),
                        br(),
                        br(),
                        br(),
                        div(style="display:inline-block;width:95%;text-align:center;",
                            actionButton("fitspict",
                                         label = " Fit SPiCT",
                                         style="color: #fff; background-color: #d35400; border-color: #d35400",
                                         icon = icon("fish", "fa-1.5x")),
                            actionButton("resetspict",
                                         label = " Reset",
                                         style="color: #fff; background-color: #d35400; border-color: #d35400",
                                         icon = icon("refresh", "fa-1.5x")
                                         )
                            ),
                        br(),
                        br(),
                        h3("Estimation settings"),
                        tags$hr(),
                        br(),
                        ## seed
                        fluidRow(
                            column(
                                6,
                        numericInput(inputId = "seed",
                                     label = "Seed value",
                                     min = 1,
                                     max = Inf,
                                     value = 1234,
                                     width = '100%')
                        ),
                        column(
                            6,"Set a seed value for reproducible results."
                        )
                        ),
                        br(),
                        fluidRow(
                            column(6,
                                   ## optimiser
                                   selectInput(
                                       "optimiser",
                                       "Optimiser",
                                       c("nlminb","optim"),
                                       "nlminb")
                                   ),
                            column(6,
                                   ## optim method
                                   selectInput(
                                       "optimMethod",
                                       "optim method",
                                       c("BFGS","SANN"),
                                       "BFGS")
                                   )
                        ),
                        br(),
                        fluidRow(
                            column(6,
                                   ## optimiser control - maxiter
                                   numericInput(
                                       "itermax",
                                       "Optimiser control - iter.max",
                                       1e4)
                                   ),
                            column(6,
                                   ## optimiser control - evalmax
                                   numericInput(
                                       "evalmax",
                                       "Optimiser control - eval.max",
                                       1e4)
                                   )
                        ),
                        br()
                    ),
                    mainPanel(
                        h3("Model convergence"),
                        tags$hr(),
                        div(style="display:inline-block;width:60%;text-align:center;font-weight:bold;",
                            column(6,),
                            verbatimTextOutput("convergence"),
                            ),
                        br(),
                        br(),
                        h3("Main SPiCT plots"),
                        tags$hr(),
                        plotOutput("plot2",height="800px"),
                        br(),
                        br(),
                        h3("Summary of model fit"),
                        tags$hr(),
                        verbatimTextOutput("fit"),
                        br(),
                        br(),
                        h3("Additional plots"),
                        tags$hr(),
                        plotOutput("plotAdd",height="800px")

                    ))
            ),

            tabPanel(
                "Diagnostics", id = "diag",
                ##-----------------------------------------------------------
                br(),
                headerPanel(title = "Check model diagnostics"),
                br(),
                tags$style(
                         HTML("hr{border-top: 2px solid #d35400;}")),
                sidebarLayout(
                    sidebarPanel(
                        id="sidebar",
                        "Checking the model diagnostics is one of the most important steps in fish stock assessment. Violated model assumptions can invalidate model results and conclusions.",
                        h3("Retrospective analysis"),
                        tags$hr(),
                        div(style="display:inline-block;width:95%;text-align: center;",
                            actionButton("runretro",
                                         label = " Run retro",
                                         style="color: #fff; background-color: #d35400; border-color: #d35400",
                                         icon = icon("filter", "fa-1.5x")),
                            actionButton("resetretro",
                                         label = " Reset",
                                         style="color: #fff; background-color: #d35400; border-color: #d35400",
                                         icon = icon("refresh", "fa-1.5x")
                                         )
                            ),
                        br(),
                        br(),
                        ## Number of retro years
                        uiOutput("nretroyear"),
                        "Number of years to remove in the retrospective analysis. Make sure that enough years remain for the model fitting.",
                        br(),
                        br(),
                        h3("Sensitivity analysis to inital values"),
                        tags$hr(),
                        div(style="display:inline-block;width:95%;text-align: center;",
                            actionButton("runsensi",
                                         label = " Run check.ini",
                                         style="color: #fff; background-color: #d35400; border-color: #d35400",
                                         icon = icon("balance-scale", "fa-1.5x")),
                            actionButton("resetsensi",
                                         label = " Reset",
                                         style="color: #fff; background-color: #d35400; border-color: #d35400",
                                         icon = icon("refresh", "fa-1.5x")
                                         )
                            ),
                        br(),
                        br(),
                        ## Number of trials
                        uiOutput("nsensi"),
                        "Number of trials for the sensitivity analysis.",
                        br()
                    ),
                    ## Show a plot of the generated distribution
                    mainPanel(
                        h3("Priors"),
                        tags$hr(),
                        plotOutput("plotPriors",height="350px"),
                        br(),
                        verbatimTextOutput("sumPriors"),
                        br(),
                        br(),
                        h3("Diagnostics"),
                        tags$hr(),
                        plotOutput("plotDiag",height="950px"),
                        br(),
                        br(),
                        verbatimTextOutput("diag"),
                        br(),
                        br(),
                        h3("Retrospective analysis"),
                        tags$hr(),
                        plotOutput("plotRetro",height="900px"),
                        br(),
                        br(),
                        h3("Sensitivity analysis"),
                        tags$hr(),
                        verbatimTextOutput("sensi"),
                        br()
                    ))
            ),

            tabPanel(
                "Management scenarios", id = "management",
                ##-----------------------------------------------------------
                br(),
                headerPanel(title = "Explore management scenarios"),
                br(),
                tags$style(
                         HTML("hr{border-top: 2px solid #d35400;}")),
                sidebarLayout(
                    sidebarPanel(
                        id="sidebar",
                        "Comparing the implications of alternative management strategies is crucial for sustainable fisheries management. SPiCT includes 8 pre-defined harvest control rules which can be selected below. Find more guidance on fisheries management in the SPiCT guidelines",
                        a("(link).",target="_blank",href="spict_guidelines.pdf"),
                        br(),
                        div(
                            style="display:inline-block;width:95%;text-align: center;",
                            actionButton("runmana",
                                         label = " Run Manage",
                                         style="color: #fff; background-color: #d35400; border-color: #d35400",
                                         icon = icon("chart-line", "fa-1.5x")),
                            actionButton("resetmana",
                                         label = " Reset",
                                         style="color: #fff; background-color: #d35400; border-color: #d35400",
                                         icon = icon("refresh", "fa-1.5x")
                                         )
                        ),
                        br(),
                        br(),
                        br(),
                        br(),
                        h3("Management settings"),
                        tags$hr(),
                        br(),
                        ## scenarios
                        selectInput(
                            inputId = "scenarios",
                            label = "Management scenarios",
                            choices = c("currentCatch" = 1,
                                        "currentF" = 2,
                                        "Fmsy" = 3,
                                        "noF" = 4,
                                        "reduceF25" = 5,
                                        "increaseF25" = 6,
                                        "msyHockeyStick" = 7,
                                        "ices" = 8),
                            selected = c(1,3),
                            multiple = TRUE,
                            width = '50%'),
                        br(),
                        br(),
                        ## management interval
                        div(
                            style="display:inline-block;width:70%;text-align:center;font-weight:bold;",
                            column(4,),
                            uiOutput("maninterval2")
                        ),
                        br(),
                        br(),
                        ## management evaluation time
                        uiOutput("maneval2"),
                        br(),
                       br(),
                       fluidRow(
                           column(
                               6,
                        ## Intermediate period catch
                        numericInput(
                            inputId = "ipc",
                            label = "Catch during intermediate period",
                            value = NULL,
                            min = 0)
                        ),
                           column(
                               6,
                        ## fractile catch for TAC
                        numericInput(
                            inputId = "fractileCatch",
                            label = "Fractile of predicted catch distribution",
                            value = 0.5,
                            min = 0,
                            max = 1)
                        )),
                        br(),
                        br()
                    ),
                    ##
                    mainPanel(
                        h3("Management times"),
                        tags$hr(),
                        verbatimTextOutput(outputId = "mantimeline2"),
                        br(),
                        h3("Management plot"),
                        tags$hr(),
                        plotOutput("plotMana",height="900px"),
                        br(),
                        br(),
                        h3("Management summary"),
                        tags$hr(),
                        verbatimTextOutput("mana"),
                        br(),
                        br(),
                        h3("Total allowable catch (TAC)"),
                        tags$hr(),
                        verbatimTextOutput("tacs"),
                        br()
                    ))
            ),

            tabPanel(
                "Summary", id = "summary",
                ##-----------------------------------------------------------
                br(),
                headerPanel(title = "Download assessment results"),
                br(),
                tags$style(
                         HTML("hr{border-top: 2px solid #d35400;}")),
                sidebarLayout(
                    ##               left = "20%", right = "auto", bottom = "auto",
                    ##               width = "80%", height = "auto",
                    sidebarPanel(id="sidebar",
                                 br(),
                                 "Generate an Rmarkdown-based assessment report summarising the results of the assessment of your data:",
                                 br(),
                                 br(),
                                 actionButton("generateReport", "Generate Assessment Report",
                                              style="color: #fff; background-color: #d35400; border-color: #d35400",
                                              icon = icon("file", "fa-1.5x")),
                                 br(),
                                 br(),
                                 "Download the assessment report:",
                                 br(),
                                 br(),
                                 conditionalPanel(condition = "output.reportbuilt",
                                                  downloadButton("downloadReport",
                                                                 style="color: #fff; background-color: #d35400; border-color: #d35400",
                                                                 "Download Assessment Report")),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 'Download all estimated parameters as a "csv" file:',
                                 br(),
                                 br(),
                                 downloadButton("allTables",
                                                style="color: #fff; background-color: #d35400; border-color: #d35400",
                                                label = "Download all tables"),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 'Download all graphs as a pdf files in a "zip" archive:',
                                 br(),
                                 br(),
                                 downloadButton("allGraphs",
                                                style="color: #fff; background-color: #d35400; border-color: #d35400",
                                                label = "Download all graphs"),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 'Download all data as a "RData" file:',
                                 br(),
                                 br(),
                                 downloadButton("allData",
                                                style="color: #fff; background-color: #d35400; border-color: #d35400",
                                                label = "Download all data"),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br()
                                 ),
                    mainPanel(
                        div(style="display:inline-block;width:100%;text-align:center;",
                            column(8, offset=2,
                                   h3("Parameter estimates"),
                                   tags$hr(),
                                   verbatimTextOutput("sumParest"),
                                   br(),
                                   h3("Stochastic reference points"),
                                   tags$hr(),
                                   verbatimTextOutput("sumSrefpoints"),
                                   br(),
                                   h3("Deterministic reference points"),
                                   tags$hr(),
                                   verbatimTextOutput("sumDrefpoints"),
                                   br(),
                                   h3("States"),
                                   tags$hr(),
                                   verbatimTextOutput("sumStates"),
                                   br(),
                                   h3("Predictions"),
                                   tags$hr(),
                                   verbatimTextOutput("sumPredictions"),
                                   br(),
                                   h3("Diagnostics"),
                                   tags$hr(),
                                   verbatimTextOutput("sumDiag"),
                                   br(),
                                   h3("Sensitivity analysis to initial values"),
                                   tags$hr(),
                                   verbatimTextOutput("sumIni"),
                                   br(),
                                   h3("Retrospective analysis"),
                                   tags$hr(),
                                   plotOutput("plotRetroSum",height="900px"),
                                   br(),
                                   h3("Management"),
                                   tags$hr(),
                                   verbatimTextOutput("sumMana"),
                                   br(),
                                   h3("Full SPiCT plot"),
                                   tags$hr(),
                                   plotOutput("plotAll",height="900px"),
                                   br(),
                                   h3("Full SPiCT management plot"),
                                   tags$hr(),
                                   plotOutput("plotMana2",height="900px"),
                                   br())
                            )
                    )
                )
            ),

            tabPanel(
                "References",
                id = "references",
                br(),
                h2("Peer-reviewed articles"),
                tags$hr(),
                div(style = "font-size:15px",
                    "Mildenberger, T. K., Berg, C. W., Pedersen, M. W., Kokkalis, A., & Nielsen, J. R. (2020). Time-variant productivity in biomass dynamic models on seasonal and long-term scales. ICES Journal of Marine Science, 77(1), 174-187. ",a("Link",href="https://academic.oup.com/icesjms/article/77/1/174/5572245"),
                    br(),
                    br(),
                    "Pedersen, M. W., & Berg, C. W. (2017). A stochastic surplus production model in continuous time. Fish and Fisheries, 18(2), 226-243. ",a("Link",href="https://github.com/DTUAqua/spict/blob/master/spict/inst/spict.pdf")
                    ),
                tags$hr(),
                br(),
                br(),
                h2("Other documentation"),
                tags$hr(),
                div(style = "font-size:15px",
                    "Mildenberger, T. K. (2020). Tutorial for spictapp: The Shiny app for the Stochastic Production model in Continuous Time (SPiCT). spictapp vignette ",
                    a("(link)",target="_blank",href="spict_app_tutorial.pdf"),'.',
                    br(),
                    br(),
                    "Pedersen, M. W., Kokkalis, A., Mildenberger, T. K., Berg, C. W. (2020). Handbook for the Stochastic Production model in Continuous Time (SPiCT). SPiCT package vignette ",
                    a("(link)",target="_blank",href="spict_handbook.pdf"),'.',
                    br(),
                    br(),
                    "Mildenberger, T. K., Kokkalis, A., Berg, C.W. (2020). Guidelines for the Stochastic Production model in Continuous Time (SPiCT). SPiCT package vignette ",
                    a("(link)",target="_blank",href="spict_guidelines.pdf"),'.'
                    ),
                tags$hr(),
                br(),
                br(),
                br(),
                br()
            ),

            tabPanel(
                "About",
                id = "about",
                div(style = "font-size:15px", br(),
                    HTML('<span style="color:#d35400;">spictapp </span>'),
                    ' is the click-based Shiny app for the SPiCT package. SPiCT is the Stochastic Production model in Continuous Time developed by Martin W. Pedersen and Casper W. Berg (2017). This app allows you to do a complete stock assessment of your own data with SPiCT including the estimation of reference levels, current and future stock status, sensitivity analysis to intial values, retrospective analysis, and the exploration of different management strategies. The software is licensed with the GPL-3.0 License',a("(link)",target="_blank",href="license.txt"),
                    'and the latest release has the version number ',
                    HTML('<span style="color:#d35400;"> 0.9.1 </span>'),'.',
                    br(), br(), tags$h2("Download"), tags$hr(),
                    'To be able to use "spictapp", please download the app from GitHub ',
                    a("(link)",href="https://github.com/tokami/apps/spictapp"),
                    'After the download, open the spictapp directory and doubleclick on ',
                    HTML('<span style="color:#d35400;"> spictapp_win </span>'),
                    ' for windows computers and ',
                    HTML('<span style="color:#d35400;"> spictapp </span>'),
                    ' for linux and mac computers. Note that the start time of the app can be a bit longer when used for the first time, as it installs all required R packages.',
                    br(), br(), tags$h2("Questions/Issues"), tags$hr(),
                    "In case you have questions or find bugs, please write an email to Tobias K. Mildenberger ",
                    a("(email)",href="mailto:t.k.mildenberger@gmail.com"),
                    "or post an issue on GitHub ",
                    a("(link)",href="https://github.com/tokami/apps/issues"),
                    ". If you want to be updated with the development of the application and underlying R package (SPiCT) or want to discuss with spictapp and SPiCT users and developers, follow the project on ResearchGate ",
                    a("(link)", href="https://www.researchgate.net/project/Stochastic-production-model-in-continuous-time-SPiCT"),
                    ".", br(), br(), tags$h2("Developer team"), tags$hr(), box(title = div(style="font-weight:bold",HTML("Tobias K. Mildenberger<br/>")),
                                                                               status = "primary",
                                                                               solidHeader = TRUE,
                                                                               collapsible = FALSE,
                                                                               background="black",
                                                                               width = 6,
                                                                               height = 1,
                                                                               fluidRow(column(width = 4, align = "center", imageOutput("tobm")),
                                                                                        column(width = 8, align = "left", HTML("<br/>Creator, Author, Maintainer<br/><br/>DTU AQUA<br/>National Institute of Aquatic Resources<br/>Technical University of Denmark<br/>Kemitorvet<br/>2800 Kgs. Lyngby<br/>Denmark"))
                                                                                        )),
                    box(title = div(style="font-weight:bold", HTML("Alexandros Kokkalis<br/>")),
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = FALSE,
                        width = 6,
                        height = 1,
                        fluidRow(column(width = 4, align = "center", imageOutput("alko")),
                                 column(width = 8, align = "left", HTML("<br/>Author<br/><br/>DTU AQUA<br/>National Institute of Aquatic Resources<br/>Technical University of Denmark<br/>Kemitorvet<br/>2800 Kgs. Lyngby<br/>Denmark")))
                        ),),
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
)
